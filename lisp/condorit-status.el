;;; condorit-status.el --- The status buffer  -*- lexical-binding: t; -*-

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
;; The main condorit buffer: held jobs, the queue grouped by cluster,
;; recently finished jobs and a pool summary.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'condorit-process)
(require 'condorit-classad)
(require 'condorit-faces)
(require 'condorit-base)
(require 'condorit-jobs)
(require 'condorit-actions)

(defcustom condorit-history-window (* 24 3600)
  "Seconds of history shown under \"Recently finished\"."
  :type 'natnum
  :group 'condorit)

(defcustom condorit-history-limit 1000
  "Maximum number of jobs shown under \"Recently finished\"."
  :type 'natnum
  :group 'condorit)

(defcustom condorit-auto-refresh-interval nil
  "Seconds between automatic refreshes of visible status buffers, or nil.
Setting it with `setopt' or Customize takes effect at once."
  :type '(choice (const :tag "Off" nil) (natnum :tag "Seconds"))
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (when (fboundp 'condorit-status--restart-timer)
           (condorit-status--restart-timer)))
  :group 'condorit)

(defcustom condorit-notify-function #'condorit-status-notify-message
  "Function called with a message when your jobs change state.
The status view compares each refresh with the previous one and
reports jobs that became held and clusters whose jobs finished.  The
default shows the message in the echo area.  For a desktop
notification:

  (setq condorit-notify-function
        (lambda (text) (notifications-notify :title \"condorit\" :body text)))

Set it to `ignore' for no notifications.  Changes are only noticed on a
refresh; see `condorit-auto-refresh-interval'."
  :type 'function
  :group 'condorit)

;;;; Commands

(defun condorit-status--queue-args (options)
  "Return condor_q arguments for the status buffer with OPTIONS."
  (append (list "-json" "-attributes" (condorit-jobs-attributes))
          (condorit-expand-args options)))

(defun condorit-status--history-args (options user since)
  "Return condor_history arguments for jobs that left the queue after SINCE.
OPTIONS are the status buffer options; unless they contain
\"-allusers\", only USER's jobs are asked for."
  (append (list "-json" "-attributes" (condorit-jobs-attributes)
                "-since" (format "EnteredCurrentStatus <= %d" since)
                "-limit" (number-to-string condorit-history-limit))
          (when-let* ((constraint (seq-find (lambda (o) (string-prefix-p "-constraint=" o))
                                            options)))
            (condorit-expand-args (list constraint)))
          (unless (member "-allusers" options)
            (list user))))

(defun condorit-status--commands ()
  "Return the commands a refresh of the status buffer runs."
  (let ((since (- (floor (float-time)) condorit-history-window)))
    `((jobs "condor_q" ,(condorit-status--queue-args condorit-options)
            ,@(and (member "-global" condorit-options)
                   (list (lambda (stdout)
                           (condorit-classad-mark-schedds (condorit-classad-parse stdout))))))
      (finished "condor_history"
                ,(condorit-status--history-args condorit-options condorit--user since))
      (slots "condor_status"
             ("-json" "-attributes" ,(string-join condorit-classad-slot-attributes ",")))
      ;; -allusers: plain -long lists only currently active submitters.
      (userprio "condor_userprio" ("-allusers" "-long") condorit-classad-parse-userprio))))

;;;; Summaries

(defun condorit-status--group-owners (jobs)
  "Group JOBS by Owner into ((OWNER . JOBS) ...), sorted by owner."
  (sort (seq-group-by (lambda (job) (or (plist-get job :Owner) "?")) jobs)
        (lambda (a b) (string< (car a) (car b)))))

(defun condorit-status--group-schedds (jobs)
  "Group JOBS by their schedd into ((SCHEDD . JOBS) ...), sorted by name."
  (sort (seq-group-by (lambda (job) (or (plist-get job :condorit-schedd) "?")) jobs)
        (lambda (a b) (string< (car a) (car b)))))

(defun condorit-status--user-priority (ads user)
  "Return USER's effective priority from condor_userprio ADS, or nil.
condor_userprio's Priority already includes the priority factor.
USER submits as \"USER@DOMAIN\", and through accounting groups or
nice_user as e.g. \"group_physics.USER@DOMAIN\" or
\"nice-user.USER@DOMAIN\".  The plain submitter wins; otherwise the
first prefixed one is used."
  (let* ((plain (concat "\\`" (regexp-quote user) "@"))
         (prefixed (concat "\\`[^@]*\\." (regexp-quote user) "@"))
         (named (lambda (regexp)
                  (seq-find (lambda (ad)
                              (let ((name (plist-get ad :Name)))
                                (and (stringp name) (string-match-p regexp name))))
                            ads))))
    (when-let* ((ad (or (funcall named plain) (funcall named prefixed))))
      (plist-get ad :Priority))))

;;;; Notifications

(defvar-local condorit-status--states nil
  "Hash table: job id -> state of your queued jobs at the last refresh.")

(defun condorit-status--job-table (jobs user)
  "Return a hash table: job id -> job, for the JOBS owned by USER."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (job jobs)
      (when (or (null user) (equal (plist-get job :Owner) user))
        (puthash (condorit-classad-job-id job) job table)))
    table))

(defun condorit-status--changes (old-states jobs finished user)
  "Describe how USER's jobs changed since OLD-STATES.
OLD-STATES maps job ids to their state at the previous refresh; JOBS
are the queued jobs now and FINISHED those in the history.  Only jobs
that became held or that the history shows as finished count, so a
job hidden by new options is not reported.  Return one string per
cluster, e.g. \"cluster 1240 done (4 completed, 1 failed)\"."
  (let ((queued (condorit-status--job-table jobs user))
        (left (condorit-status--job-table finished user))
        (counts (make-hash-table :test #'equal)))
    (maphash
     (lambda (id old-state)
       (let* ((now (gethash id queued))
              (gone (and (not now) (gethash id left)))
              (event (cond ((and now (eq (condorit-classad-state now) 'held)
                                 (not (eq old-state 'held)))
                            'held)
                           ((null gone) nil)
                           ((condorit-classad-failed-p gone) 'failed)
                           ((eq (condorit-classad-state gone) 'completed) 'completed)
                           ((eq (condorit-classad-state gone) 'removed) 'removed))))
         (when event
           (let ((cluster (condorit-classad-cluster-id (or now gone))))
             (cl-incf (alist-get event (gethash cluster counts) 0))))))
     old-states)
    (mapcar
     (lambda (cluster)
       (let* ((events (gethash cluster counts))
              (parts (cl-loop for event in '(held completed failed removed)
                              for n = (alist-get event events)
                              when n collect (format "%d %s" n event)))
              (done (not (seq-some (lambda (job)
                                     (equal (condorit-classad-cluster-id job) cluster))
                                   (hash-table-values queued)))))
         (if done
             (format "cluster %s done (%s)" cluster (string-join parts ", "))
           (format "cluster %s: %s" cluster (string-join parts ", ")))))
     (sort (hash-table-keys counts) #'string-version-lessp))))

(defun condorit-status-notify-message (text)
  "Show TEXT about changed jobs in the echo area."
  (message "condorit: %s" text))

(defun condorit-status--notify ()
  "Report how your jobs changed since the previous refresh.
Nothing is reported on the first refresh."
  (let ((jobs (plist-get condorit--data :jobs)))
    (when condorit-status--states
      (when-let* ((changes (condorit-status--changes condorit-status--states jobs
                                                    (plist-get condorit--data :finished)
                                                    condorit--user)))
        (funcall condorit-notify-function (string-join changes " · "))))
    (let ((states (make-hash-table :test #'equal)))
      (maphash (lambda (id job) (puthash id (condorit-classad-state job) states))
               (condorit-status--job-table jobs condorit--user))
      (setq condorit-status--states states))))

;;;; Sections

(defun condorit-status--insert-header (data)
  "Insert the header lines: schedd, user, refresh time and pool from DATA."
  (insert (format "%s %s   %s %s   %s\n" (condorit--label "Schedd:")
                  (or condorit--host "?") (condorit--propertize "User:" 'condorit-header-label)
                  (or condorit--user "?")
                  (condorit--refresh-state)))
  (when data
    (let ((pool (condorit-classad-pool-summary (plist-get data :slots)))
          (idle (cl-count 'idle (plist-get data :jobs) :key #'condorit-classad-state))
          (priority (condorit-status--user-priority (plist-get data :userprio)
                                                   (or condorit--user ""))))
      (insert (format "%s %s/%s CPUs busy · %s machine(s) · %s slot(s) claimed · %s %s idle %s%s\n"
                      (condorit--label "Pool:")
                      (condorit--number (plist-get pool :cpus-used))
                      (condorit--number (plist-get pool :cpus-total))
                      (condorit--number (plist-get pool :machines))
                      (condorit--number (plist-get pool :jobs))
                      (condorit--number idle)
                      ;; Only the jobs this view asked for, so say which.
                      (if (member "-allusers" condorit-options) "job(s)" "of your jobs")
                      (if (member "-global" condorit-options) "in the pool" "on this schedd")
                      (if priority
                          (concat " · priority "
                                  (condorit--propertize (format "%.1f" priority) 'condorit-number)
                                  " (lower is better)")
                        "")))))
  (when condorit-show-legend
    (insert (condorit--label "States:") " " (condorit-state-legend) "\n"))
  (condorit--insert-options)
  (condorit-jobs--insert-sort-line)
  (condorit--insert-error)
  (insert "\n"))

(defun condorit-status--insert-held (jobs)
  "Insert the section of held JOBS with their reasons, if there are any."
  (let ((held (seq-filter (lambda (job) (eq (condorit-classad-state job) 'held)) jobs)))
    (when held
      (magit-insert-section (condorit-held)
        (magit-insert-heading (format "Held (%d)" (length held)))
        (insert (condorit--header-row "  " (condorit-jobs--columns)))
        (dolist (job (condorit-jobs--sort-jobs held))
          (condorit-jobs--insert-job job "  " (condorit-jobs--short-reason job)))
        (insert "\n")))))

(defun condorit-status--insert-owners (jobs now indent)
  "Insert JOBS grouped by owner under -allusers, indented by INDENT.
NOW is the current time."
  (if (member "-allusers" condorit-options)
      (pcase-dolist (`(,owner . ,owned) (condorit-status--group-owners jobs))
        (magit-insert-section (condorit-owner owner)
          (magit-insert-heading (format "%s%s (%d)" indent owner (length owned)))
          (condorit-jobs--insert-clusters owned now (concat indent "  ") nil)))
    (condorit-jobs--insert-clusters jobs now indent nil)))

(defun condorit-status--insert-queue (jobs now)
  "Insert the queue section of JOBS.
They are grouped by schedd under -global, then by owner under -allusers.
NOW is the current time."
  (let ((allusers (member "-allusers" condorit-options))
        (global (member "-global" condorit-options)))
    (magit-insert-section (condorit-queue)
      (magit-insert-heading
        (format "%s%s (%d)" (if allusers "All jobs" "My jobs")
                (if global ", all schedds" "") (length jobs)))
      (when jobs
        ;; Clusters are indented one level per grouping above them.
        (insert (condorit--header-row
                 (make-string (* 2 (+ 1 (if allusers 1 0) (if global 1 0))) ?\s)
                 (condorit-jobs--columns))))
      (cond ((null jobs) (insert "  No jobs in the queue.\n"))
            (global
             (pcase-dolist (`(,schedd . ,queued) (condorit-status--group-schedds jobs))
               (magit-insert-section (condorit-schedd schedd)
                 (magit-insert-heading (format "  %s (%d)" schedd (length queued)))
                 (condorit-status--insert-owners queued now "    "))))
            (t (condorit-status--insert-owners jobs now "  ")))
      (insert "\n"))))

(defun condorit-status--insert-finished (jobs now)
  "Insert the collapsed section of recently finished JOBS.
NOW is the current time."
  (magit-insert-section (condorit-finished nil t)
    (magit-insert-heading
      (format "Recently finished (last %dh%s) (%d%s)"
              (/ condorit-history-window 3600)
              ;; condor_history has no -global.
              (if (member "-global" condorit-options) ", this schedd only" "")
              (length jobs)
              (if (>= (length jobs) condorit-history-limit)
                  ", the newest only: see condorit-history-limit"
                "")))
    (if jobs
        (progn (insert (condorit--header-row "  " (condorit-jobs--columns)))
               (condorit-jobs--insert-clusters jobs now "  " t))
      (insert "  Nothing finished recently.\n"))))

(defun condorit-status--insert (data)
  "Insert the status buffer's content from DATA."
  (condorit-jobs--index (append (plist-get data :finished) (plist-get data :jobs)))
  (setq condorit-jobs--capacities (condorit-classad-slot-capacities (plist-get data :slots)))
  (condorit-status--insert-header data)
  (when data
    (let ((now (plist-get data :time))
          (jobs (plist-get data :jobs)))
      (condorit-status--insert-held jobs)
      (condorit-status--insert-queue jobs now)
      (condorit-status--insert-finished (plist-get data :finished) now))))

;;;; Auto-refresh

(defvar condorit-status--timer nil "Timer refreshing the visible status buffers.")

(defun condorit-status--auto-refresh (buffer)
  "Refresh BUFFER if it is visible, not refreshing and has no region."
  (with-current-buffer buffer
    ;; A redraw would move the mark of a region being selected.
    (when (and (get-buffer-window buffer t)
               (not condorit--pending)
               (not (region-active-p)))
      (condorit-refresh))))

(defun condorit-status--tick ()
  "Refresh the visible status buffers, see `condorit-status--auto-refresh'."
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'condorit-status-mode)
      (condorit-status--auto-refresh buffer))))

(defun condorit-status--restart-timer ()
  "Restart the auto-refresh timer for `condorit-auto-refresh-interval'."
  (when condorit-status--timer
    (cancel-timer condorit-status--timer))
  (setq condorit-status--timer
        (and condorit-auto-refresh-interval
             (run-with-timer condorit-auto-refresh-interval condorit-auto-refresh-interval
                             #'condorit-status--tick))))

;;;; Mode and entry point

(defvar-keymap condorit-status-mode-map
  :doc "Keys for acting on queued jobs."
  :parent condorit-jobs-mode-map
  "a" #'condorit-analyze
  "k" #'condorit-remove
  "H" #'condorit-hold
  "R" #'condorit-release
  "e" #'condorit-edit
  "S" #'condorit-ssh-to-job)

(define-derived-mode condorit-status-mode condorit-jobs-mode "Condorit"
  "Major mode for the HTCondor status buffer.

\\{condorit-status-mode-map}"
  (setq-local condorit--fetch-function #'condorit-status--commands
              condorit--insert-function #'condorit-status--insert)
  (add-hook 'condorit-after-refresh-hook #'condorit-status--notify nil t)
  (unless (or condorit-status--timer (not condorit-auto-refresh-interval))
    (condorit-status--restart-timer)))

;;;###autoload
(defun condorit-status (&optional directory)
  "Show the HTCondor status buffer for the host of DIRECTORY.
DIRECTORY defaults to `default-directory'."
  (interactive)
  (let* ((default-directory (or directory default-directory))
         (buffer (condorit--view-buffer (condorit--buffer-name "condorit")
                                       #'condorit-status-mode)))
    (with-current-buffer buffer
      ;; Asked once per buffer: these run synchronously.
      (unless condorit--host
        (setq condorit--host (condorit--detect-schedd)
              condorit--user (condorit--detect-user)))
      (condorit-refresh))
    ;; The entry point, like `magit-status': take over this window.
    (pop-to-buffer-same-window buffer)))

(provide 'condorit-status)
;;; condorit-status.el ends here
