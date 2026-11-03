;;; condorit-actions.el --- Commands on jobs for condorit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Hohn

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The commands that act on the jobs at point: show the full ClassAd,
;; analyze, remove, hold, release, edit, and open a job's log files.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit-section)
(require 'transient)
(require 'condorit-process)
(require 'condorit-classad)
(require 'condorit-base)
(require 'condorit-jobs)

(declare-function term-mode "term" ())
(declare-function term-check-proc "term" (buffer))
(declare-function term-emulate-terminal "term" (proc str))
(declare-function term-sentinel "term" (proc msg))
(defvar term-term-name)
(defvar term-height)
(defvar term-width)
(defvar term-ptyp)
(declare-function term-char-mode "term" ())

(defcustom condorit-confirm-remove t
  "Whether removing jobs asks for confirmation first."
  :type 'boolean
  :group 'condorit)

(defun condorit-actions--args (args ids)
  "Return the arguments for transient ARGS and job IDS, e.g. of condor_rm.
IDS of another schedd are named with -name."
  (pcase-let ((`(,schedd . ,ids) (condorit-jobs--split-ids ids)))
    (append (condorit-jobs--schedd-args schedd) (condorit-expand-args args) ids)))

(defun condorit-actions--qedit-args (ids attribute value)
  "Return condor_qedit arguments setting ATTRIBUTE to VALUE for job IDS."
  (pcase-let ((`(,schedd . ,ids) (condorit-jobs--split-ids ids)))
    (append (condorit-jobs--schedd-args schedd)
            (list "-constraint" (condorit-jobs--ids-constraint ids) attribute value))))

;;;; ClassAd and analysis

(defun condorit-show-classad (id)
  "Show the full ClassAd of job ID, from the queue or else the history."
  (interactive (list (condorit-job-id-at-point)))
  (pcase-let ((`(,schedd . ,local) (condorit-classad-split-id id)))
    (condorit--show-ad
     (condorit--buffer-name "condorit-classad" id)
     (or (condorit--fetch-ad "condor_q" (append (condorit-jobs--schedd-args schedd)
                                               (list "-json" local)))
         (condorit--fetch-ad "condor_history" (append (condorit-jobs--schedd-args schedd)
                                                     (list "-json" "-limit" "1" local)))
         (user-error "Job %s is neither in the queue nor in the history" id)))))

(defun condorit-visit ()
  "Show the ClassAd of the job at point, or fold the section at point."
  (interactive)
  (let ((section (magit-current-section)))
    (if (and section (eq (oref section type) 'condorit-job))
        (condorit-show-classad (oref section value))
      (magit-section-toggle section))))

(defun condorit-actions--check-running (id)
  "Signal a `user-error' unless job or cluster ID has a running job.
Jobs this buffer does not know about are let through."
  (when condorit-jobs--by-id
    (let ((jobs (seq-filter (lambda (job)
                              (or (equal (condorit-classad-job-id job) id)
                                  (equal (condorit-classad-cluster-id job) id)))
                            (hash-table-values condorit-jobs--by-id))))
      (when (and jobs (not (seq-some (lambda (job) (eq (condorit-classad-state job) 'running))
                                     jobs)))
        (user-error "%s is not running; there is no shell to open"
                    (if (string-search "." id)
                        (concat "Job " id)
                      (concat "No job of cluster " id)))))))

(defcustom condorit-terminal-function #'condorit-term
  "Function that runs an interactive command in a terminal buffer.
It is called with a buffer NAME, a PROGRAM and its list of ARGS.  It
must run PROGRAM on the host of `default-directory', which is remote
for a view opened over TRAMP.  The default uses the built-in `term'."
  :type 'function
  :group 'condorit)

(defun condorit-term (name program args)
  "Run PROGRAM with ARGS in a `term' buffer called NAME and show it.
PROGRAM runs on the host of `default-directory', also through TRAMP.
If a session still runs in that buffer, just show it."
  (require 'term)
  (let ((buffer (get-buffer-create name))
        (dir default-directory))
    (unless (term-check-proc buffer)
      (with-current-buffer buffer
        (setq default-directory dir)
        (term-mode)
        ;; Started as `term-exec' does, but with `start-file-process',
        ;; which TRAMP can run on a remote host.  stty sets up the
        ;; terminal the way term expects it, e.g. with CR before LF.
        (let* ((process-environment
                (append (list (format "TERM=%s" term-term-name)
                              (format "LINES=%d" term-height)
                              (format "COLUMNS=%d" term-width))
                        process-environment))
               (process-connection-type t)
               (process (apply #'start-file-process name buffer "/bin/sh" "-c"
                               (format "stty -nl echo rows %d columns %d sane 2>/dev/null; exec \"$@\""
                                       term-height term-width)
                               ".." program args)))
          ;; term decodes the output itself; TRAMP would pick a coding
          ;; system of its own, e.g. utf-8-dos, which drops the CRs.
          (set-process-coding-system process 'binary
                                     (cdr (process-coding-system process)))
          (setq-local term-ptyp t)
          (goto-char (point-max))
          (set-marker (process-mark process) (point))
          (set-process-filter process #'term-emulate-terminal)
          (set-process-sentinel process #'term-sentinel))
        (term-char-mode)))
    (pop-to-buffer buffer)))

(defun condorit-ssh-to-job (id)
  "Open a shell inside running job (or cluster) ID with condor_ssh_to_job.
The shell runs on the execute node, in the job's scratch directory, in
a terminal buffer made by `condorit-terminal-function'."
  (interactive (list (condorit-job-id-at-point)))
  (condorit-actions--check-running id)
  (let ((args (condorit-actions--args nil (list id))))
    (condorit--log "condor_ssh_to_job" args nil "" "")
    (funcall condorit-terminal-function (condorit--buffer-name "condorit-ssh" id)
             "condor_ssh_to_job" args)))

(defun condorit-analyze (id)
  "Explain why job ID is not running, using condor_q -better-analyze."
  (interactive (list (condorit-job-id-at-point)))
  (let ((args (condorit-actions--args '("-better-analyze") (list id))))
    (message "Running %s…" (condorit-command-string "condor_q" args))
    (condorit-run "condor_q" args
                 (lambda (exit stdout stderr)
                   (condorit--show-output (condorit--buffer-name "condorit-analyze" id)
                                         "condor_q" args (concat stdout stderr))
                   (unless (eql exit 0)
                     (message "%s" (condorit-error-summary "condor_q" exit stdout stderr)))))))

;;;; Release and edit

(defun condorit-release (ids)
  "Release the held jobs IDS."
  (interactive (list (condorit-jobs-at-point)))
  (condorit--run-action "condor_release" (condorit-actions--args nil ids)))

(defcustom condorit-edit-attributes
  '("RequestMemory" "RequestCpus" "RequestDisk" "JobPrio")
  "Attributes offered for completion by `condorit-edit'.
Any other attribute can be typed in as well."
  :type '(repeat string)
  :group 'condorit)

(defun condorit-edit (ids attribute value)
  "Set ATTRIBUTE to the ClassAd expression VALUE for jobs IDS (condor_qedit)."
  (interactive
   (let* ((ids (condorit-jobs-at-point))
          (attribute (completing-read (format "Attribute for %s: " (condorit-jobs--describe-ids ids))
                                      condorit-edit-attributes))
          (job (and (= (length ids) 1) condorit-jobs--by-id (gethash (car ids) condorit-jobs--by-id)))
          (current (and job (plist-get job (intern (concat ":" attribute))))))
     (list ids attribute
           (read-string (format "%s (ClassAd expression; quote strings) = " attribute)
                        (and current (condorit-classad-format-value current))))))
  (when (string-blank-p value)
    (user-error "Empty value"))
  (condorit--run-action "condor_qedit" (condorit-actions--qedit-args ids attribute value)))

;;;; Remove and hold

(defun condorit-actions--remove (args ids)
  "Remove IDS with condor_rm ARGS, confirming first if configured."
  (when (or (not condorit-confirm-remove)
            (yes-or-no-p (format "Remove %s? " (condorit-jobs--describe-ids ids))))
    (condorit--run-action "condor_rm" (condorit-actions--args args ids))))

(transient-define-suffix condorit-remove-run (args)
  "Remove the jobs with the chosen options."
  (interactive (list (transient-args 'condorit-remove)))
  (condorit-actions--remove args (transient-scope 'condorit-remove)))

(transient-define-prefix condorit-remove (ids)
  "Remove jobs from the queue (condor_rm)."
  :man-page "condor_rm"
  [:description
   (lambda () (concat "Remove " (condorit-jobs--describe-ids (transient-scope 'condorit-remove))))
   ("-f" "Force removal of jobs stuck in X (removed) state" "-forcex")
   (:info (lambda ()
            (condorit--preview
             "condor_rm" (condorit-actions--args (transient-get-value)
                                                (transient-scope 'condorit-remove)))))
   ("k" "Remove" condorit-remove-run)]
  (interactive (list (condorit-jobs-at-point)))
  (transient-setup 'condorit-remove nil nil :scope ids))

(transient-define-suffix condorit-hold-run (args)
  "Hold the jobs with the chosen options."
  (interactive (list (transient-args 'condorit-hold)))
  (condorit--run-action "condor_hold"
                       (condorit-actions--args args (transient-scope 'condorit-hold))))

(transient-define-prefix condorit-hold (ids)
  "Put jobs on hold (condor_hold).
Release them later with \\<condorit-status-mode-map>\\[condorit-release]."
  :man-page "condor_hold"
  [:description
   (lambda () (concat "Hold " (condorit-jobs--describe-ids (transient-scope 'condorit-hold))))
   ("-r" "Reason, stored in the job's HoldReason" "-reason=")
   (:info (lambda ()
            (condorit--preview
             "condor_hold" (condorit-actions--args (transient-get-value)
                                                  (transient-scope 'condorit-hold)))))
   ("H" "Hold" condorit-hold-run)]
  (interactive (list (condorit-jobs-at-point)))
  (transient-setup 'condorit-hold nil nil :scope ids))

;;;; Logs

(defun condorit-actions--job-file (job attribute)
  "Return the full file name in JOB's ATTRIBUTE (:UserLog, :Out or :Err)."
  (let ((path (plist-get job attribute))
        (iwd (plist-get job :Iwd))
        (name (substring (symbol-name attribute) 1))
        (id (condorit-classad-job-id job)))
    (cond ((or (not (stringp path)) (string-empty-p path))
           (user-error "Job %s has no %s" id name))
          ((equal path "/dev/null")
           (user-error "Job %s discards it (%s = /dev/null)" id name))
          ((and (not (file-name-absolute-p path)) (not (stringp iwd)))
           (user-error "Job %s: %s is relative but Iwd is unknown" id name))
          (t (concat (or (file-remote-p default-directory) "")
                     (expand-file-name path iwd))))))

(defun condorit-actions--open-file (job attribute)
  "Open the file named by ATTRIBUTE (e.g. :Out) of JOB in `view-mode'.
\\<view-mode-map>\\[View-quit] closes it again, and kills the buffer unless it \
was already open."
  (let ((file (condorit-actions--job-file job attribute)))
    (unless (file-exists-p file)
      (user-error "%s does not exist (yet)" file))
    (let ((visited (find-buffer-visiting file)))
      (find-file-other-window file)
      (auto-revert-tail-mode 1)
      (view-mode-enter nil (unless visited #'kill-buffer)))))

(defvar condorit-tail-bytes 16384
  "How many bytes of a running job's output condor_tail first shows.")

(defvar-keymap condorit-tail-mode-map
  :parent special-mode-map
  "q" #'condorit-tail-quit)

(define-derived-mode condorit-tail-mode special-mode "Condorit-Tail"
  "Major mode following a running job's output with condor_tail -f.
\\<condorit-tail-mode-map>\\[condorit-tail-quit] closes the window and stops condor_tail.")

(defun condorit-tail-quit ()
  "Close the window and kill the buffer, which stops condor_tail."
  (interactive)
  (quit-window t))

(defun condorit-actions--tail-filter (process output)
  "Append OUTPUT of condor_tail PROCESS; windows at its end follow it."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let* ((mark (process-mark process))
             (following (seq-filter (lambda (window) (= (window-point window) mark))
                                    (get-buffer-window-list nil nil t)))
             (at-end (= (point) mark))
             (inhibit-read-only t))
        (save-excursion
          (goto-char mark)
          (insert output)
          (set-marker mark (point)))
        (when at-end (goto-char mark))
        (dolist (window following)
          (set-window-point window mark))))))

(defun condorit-actions--tail-sentinel (process event)
  "Note in PROCESS's buffer that condor_tail ended with EVENT."
  (when (and (buffer-live-p (process-buffer process))
             (not (process-live-p process)))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (propertize (format "\n[condor_tail %s]\n" (string-trim event))
                              'font-lock-face 'shadow)))))))

(defun condorit-tail (id &optional stderr)
  "Follow the standard output of running job ID with condor_tail -f.
With STDERR non-nil, follow its standard error instead.  This shows the
output while the job runs, also when it is transferred back only at the
end.  Killing the buffer stops it."
  (condorit-actions--check-running id)
  (let* ((args (condorit-actions--args
                (append (list "-f" "-maxbytes" (number-to-string condorit-tail-bytes))
                        (and stderr '("-no-stdout" "-stderr")))
                (list id)))
         (buffer (get-buffer-create
                  (condorit--buffer-name "condorit-tail" (concat id (if stderr " stderr" "")))))
         (dir default-directory))
    (unless (get-buffer-process buffer)
      (condorit--log "condor_tail" args nil "" "")
      (with-current-buffer buffer
        (condorit-tail-mode)
        (setq default-directory dir)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (concat "$ " (condorit-command-string "condor_tail" args))
                              'font-lock-face 'bold)
                  "\n\n"))
        (let* ((process-connection-type nil)
               (process (apply #'start-file-process "condor_tail" buffer "condor_tail" args)))
          (set-process-query-on-exit-flag process nil)
          (set-marker (process-mark process) (point-max))
          (set-process-filter process #'condorit-actions--tail-filter)
          (set-process-sentinel process #'condorit-actions--tail-sentinel))))
    (pop-to-buffer buffer)))

(transient-define-prefix condorit-logs (job)
  "Open a job's event log, standard output or standard error."
  [:description
   (lambda () (format "Files of job %s" (condorit-classad-job-id (transient-scope 'condorit-logs))))
   ("l" "Event log (UserLog)"
    (lambda () (interactive) (condorit-actions--open-file (transient-scope 'condorit-logs) :UserLog)))
   ("o" "Standard output (Out)"
    (lambda () (interactive) (condorit-actions--open-file (transient-scope 'condorit-logs) :Out)))
   ("e" "Standard error (Err)"
    (lambda () (interactive) (condorit-actions--open-file (transient-scope 'condorit-logs) :Err)))]
  ["While it runs (condor_tail -f)"
   ("O" "Follow standard output"
    (lambda () (interactive)
      (condorit-tail (condorit-classad-job-id (transient-scope 'condorit-logs)))))
   ("E" "Follow standard error"
    (lambda () (interactive)
      (condorit-tail (condorit-classad-job-id (transient-scope 'condorit-logs)) t)))]
  (interactive (list (condorit-job-at-point)))
  (transient-setup 'condorit-logs nil nil :scope job))

(provide 'condorit-actions)
;;; condorit-actions.el ends here
