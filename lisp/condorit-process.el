;;; condorit-process.el --- Run condor commands for condorit  -*- lexical-binding: t; -*-

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
;; Every condor command condorit runs goes through this file.  Commands
;; run on the host of `default-directory', also through TRAMP, and are
;; recorded in the command log, so users can see and copy the exact
;; command lines.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar condorit-log-max-output 4000
  "Maximum number of characters of a failed command's output kept in the log.")

(defvar condorit-log-max-lines 5
  "Maximum number of lines of a successful command's output kept in the log.
Queries print a lot of JSON that condorit has already shown in its
buffers; the rest is noted as \"[N more lines]\".  nil keeps them all,
up to `condorit-log-max-output' characters.")

(defvar condorit-log-max-size 200000
  "Size in characters beyond which the oldest log entries are dropped.")

(defconst condorit-log-buffer-name "*condorit-log*"
  "Name of the buffer logging every condor command condorit runs.")

(define-derived-mode condorit-log-mode special-mode "Condorit-Log"
  "Major mode for the log of condor commands run by condorit.")

(defun condorit--shell-quote (arg)
  "Quote ARG for a shell, leaving simple words alone for readability."
  (if (string-match-p "\\`[-[:alnum:]_.,/:=@+%]+\\'" arg)
      arg
    (concat "'" (string-replace "'" "'\\''" arg) "'")))

(defun condorit-command-string (program args)
  "Return PROGRAM and ARGS as a shell command line ready to copy."
  (mapconcat #'condorit--shell-quote (cons program args) " "))

(defun condorit-expand-args (args)
  "Split transient-style \"-flag=value\" elements of ARGS into two words."
  (mapcan (lambda (arg)
            (if (string-match "\\`\\(-[^=]+\\)=\\(.*\\)\\'" arg)
                (list (match-string 1 arg) (match-string 2 arg))
              (list arg)))
          args))

(defun condorit-error-summary (program exit stdout stderr)
  "Return a one-line description of PROGRAM failing with EXIT.
The first line of STDERR is used, or of STDOUT when STDERR is empty
\(condor_rm and friends report errors on stdout)."
  (let ((text (string-trim (if (string-blank-p stderr) stdout stderr))))
    (format "%s failed (exit %s): %s" program exit
            (if (string-empty-p text) "no output" (car (split-string text "\n"))))))

(defun condorit--truncate (string &optional max-lines)
  "Return STRING cut to MAX-LINES lines and `condorit-log-max-output' characters."
  (let* ((lines (split-string (string-trim-right string) "\n"))
         (string (if (and max-lines (> (length lines) max-lines))
                     (format "%s\n… [%d more lines]"
                             (string-join (take max-lines lines) "\n")
                             (- (length lines) max-lines))
                   string)))
    (if (> (length string) condorit-log-max-output)
        (format "%s\n… [%d more characters]"
                (substring string 0 condorit-log-max-output)
                (- (length string) condorit-log-max-output))
      string)))

(defun condorit--log-trim ()
  "Drop the oldest entries of the log beyond `condorit-log-max-size'."
  (when (> (buffer-size) condorit-log-max-size)
    (save-excursion
      (goto-char (- (point-max) condorit-log-max-size))
      (when (re-search-forward "^[0-9:]\\{8\\} \\$ " nil t)
        (delete-region (point-min) (match-beginning 0))))))

(defun condorit--log (program args exit stdout stderr)
  "Append a run of PROGRAM with ARGS to the command log.
EXIT is its exit status, or nil for a command that runs on in a
terminal; STDOUT and STDERR are its output."
  (with-current-buffer (get-buffer-create condorit-log-buffer-name)
    (unless (derived-mode-p 'condorit-log-mode)
      (condorit-log-mode))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert (propertize (format "%s $ %s" (format-time-string "%H:%M:%S")
                                    (condorit-command-string program args))
                            'face 'bold)
                (if exit (format "   [exit %s]\n" exit) "\n"))
        (dolist (output (list stdout stderr))
          (unless (string-blank-p output)
            (insert (replace-regexp-in-string
                     "^" "  " (string-trim-right
                               (condorit--truncate
                                output (and (eql exit 0) condorit-log-max-lines))))
                    "\n")))
        (condorit--log-trim)))))

(defun condorit-show-log ()
  "Show the log of condor commands condorit has run."
  (interactive)
  (let ((buffer (get-buffer-create condorit-log-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'condorit-log-mode)
        (condorit-log-mode)))
    (pop-to-buffer buffer)
    (goto-char (point-max))))

(defun condorit--check-executable (program)
  "Signal a `user-error' unless PROGRAM exists on `default-directory's host."
  (unless (executable-find program t)
    (user-error "`%s' not found on %s" program
                (or (file-remote-p default-directory 'host) "this machine"))))

(defun condorit--run-local (program args callback)
  "Run PROGRAM with ARGS on the local host, then call CALLBACK.
CALLBACK gets the exit status, stdout and stderr, with
`default-directory' as it was when PROGRAM started."
  (let* ((dir default-directory)
         (out (generate-new-buffer " *condorit-stdout*" t))
         (err (generate-new-buffer " *condorit-stderr*" t))
         (exit nil)
         (errproc nil)
         ;; stderr comes through a pipe of its own, which may close
         ;; before or after PROGRAM exits; finish once, when both happened.
         (finish
          (lambda ()
            (when (and exit (not (process-live-p errproc)) (buffer-live-p out))
              (let ((stdout (with-current-buffer out (buffer-string)))
                    (stderr (with-current-buffer err (buffer-string)))
                    (default-directory dir))
                (kill-buffer out)
                (kill-buffer err)
                (funcall callback exit stdout stderr))))))
    (make-process
     :name program :buffer out :stderr err :command (cons program args)
     :coding 'utf-8 :noquery t :connection-type 'pipe
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (setq exit (process-exit-status proc))
                   (funcall finish))))
    (setq errproc (get-buffer-process err))
    (set-process-sentinel errproc (lambda (_proc _event) (funcall finish)))))

;; On a remote host every command costs an ssh session, and running
;; them in parallel does not help: TRAMP without direct async cannot
;; start a second process while one runs, and some sshd setups set up
;; concurrent sessions one at a time (about 2s each).  So all commands
;; of a refresh run in one shell script, which prints each command's
;; exit code, stdout and stderr between marker lines.  That also keeps
;; login noise out of the output and avoids TRAMP's :stderr buffers,
;; which need a second remote process and lose output now and then.

(defun condorit--batch-script (specs marker)
  "Return a shell script running SPECS, separating their output by MARKER.
SPECS is a list of (PROGRAM ARGS).  For each it prints \"MARKER INDEX
EXIT\", the command's stdout, \"MARKER\" and its stderr, each marker on
a line of its own."
  (concat
   "t=$(mktemp -d) || exit 1\n"
   (cl-loop for (program args) in specs
            for i from 0
            concat (format "%s >\"$t/o\" 2>\"$t/e\"; x=$?\n"
                           (mapconcat (lambda (word) (shell-quote-argument word t))
                                      (cons program args) " "))
            concat (format "printf '\\n%s %d %%d\\n' \"$x\"; cat \"$t/o\"; printf '\\n%s\\n'; cat \"$t/e\"\n"
                           marker i marker))
   (format "printf '\\n%s end\\n'; rm -rf \"$t\"\n" marker)))

(defun condorit--batch-split (output marker)
  "Split OUTPUT of `condorit--batch-script' at its MARKER lines.
Return a list of (INDEX EXIT STDOUT STDERR).  Text before the first
marker, e.g. login noise, is ignored."
  (let ((head (format "\n%s \\([0-9]+\\) \\([0-9]+\\)\n" (regexp-quote marker)))
        (sep (format "\n%s\n" marker))
        (results nil)
        (pos 0))
    (while (string-match head output pos)
      (let* ((index (string-to-number (match-string 1 output)))
             (exit (string-to-number (match-string 2 output)))
             (start (match-end 0))
             (mid (string-search sep output start))
             (end (and mid (string-match (format "\n%s " (regexp-quote marker)) output mid))))
        (unless (and mid end)
          (error "Unexpected output of the batched commands"))
        (push (list index exit
                    (substring output start mid)
                    (substring output (+ mid (length sep)) end))
              results)
        (setq pos end)))
    (nreverse results)))

(defun condorit--run-remote (specs callback)
  "Run SPECS in one shell on `default-directory's remote host.
SPECS is a list of (PROGRAM ARGS).  Then call CALLBACK with a list of
\(EXIT STDOUT STDERR), one per spec, with `default-directory' as it
was when they started."
  (let ((dir default-directory)
        (out (generate-new-buffer " *condorit-stdout*" t))
        (marker (format "@condorit-%x%x@" (random most-positive-fixnum)
                        (random most-positive-fixnum))))
    (make-process
     :name "condorit" :buffer out
     :command (list "/bin/sh" "-c" (condorit--batch-script specs marker))
     :coding 'utf-8 :noquery t :connection-type 'pipe :file-handler t
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (let* ((exit (process-exit-status proc))
                (output (with-current-buffer out (buffer-string)))
                (parts (ignore-errors (condorit--batch-split output marker)))
                (default-directory dir))
           (kill-buffer out)
           (funcall callback
                    (cl-loop for i from 0 below (length specs)
                             ;; A missing part means the script itself failed.
                             collect (or (cdr (assq i parts))
                                         (list (if (eql exit 0) 1 exit) ""
                                               (if (string-blank-p output)
                                                   "the batched commands failed"
                                                 output)))))))))))

(defun condorit-run (program args callback)
  "Run PROGRAM with ARGS asynchronously on `default-directory's host.
When it exits, call CALLBACK with EXIT-CODE, STDOUT and STDERR, with
`default-directory' bound as it was when the command started.  The
command and its output are appended to the command log."
  (condorit-run-all `((nil ,program ,args))
                   (pcase-lambda (`((,_ ,_ . ,result)))
                     (apply callback result))))

(defun condorit-run-sync (program &rest args)
  "Run PROGRAM with ARGS synchronously; return (EXIT STDOUT STDERR)."
  (condorit--check-executable program)
  (let ((errfile (make-nearby-temp-file "condorit-stderr")))
    (unwind-protect
        (with-temp-buffer
          (let* ((exit (apply #'process-file program nil
                              (list t (file-local-name errfile)) nil args))
                 (stdout (buffer-string))
                 (stderr (with-temp-buffer
                           (insert-file-contents errfile)
                           (buffer-string))))
            (condorit--log program args exit stdout stderr)
            (list exit stdout stderr)))
      (delete-file errfile))))

(defun condorit-run-all (specs callback)
  "Run SPECS and call CALLBACK once all have finished.
Each spec is (KEY PROGRAM ARGS).  CALLBACK receives a list of
\(KEY PROGRAM EXIT STDOUT STDERR).  Locally the commands run in
parallel; on a remote host they run in one shell, see
`condorit--run-remote'.  Each command is logged on its own."
  (pcase-dolist (`(,_key ,program ,_args) specs)
    (condorit--check-executable program))
  (let ((finish (lambda (results)
                  (cl-loop for (key program args) in specs
                           for result in results
                           do (apply #'condorit--log program args result)
                           collect `(,key ,program ,@result)))))
    (cond ((null specs) (funcall callback nil))
          ((file-remote-p default-directory)
           (condorit--run-remote (mapcar #'cdr specs)
                                (lambda (results) (funcall callback (funcall finish results)))))
          (t
           (let ((results (make-vector (length specs) nil))
                 (remaining (length specs)))
             (cl-loop for (_key program args) in specs
                      for i from 0
                      do (let ((i i))
                           (condorit--run-local
                            program args
                            (lambda (&rest result)
                              (aset results i result)
                              (when (zerop (cl-decf remaining))
                                (funcall callback
                                         (funcall finish (append results nil)))))))))))))

(defun condorit--buffer-name (base &optional suffix)
  "Return \"*BASE*\" or \"*BASE: SUFFIX*\", with \"@HOST\" after BASE when remote."
  (let ((host (file-remote-p default-directory 'host)))
    (format "*%s%s%s*" base (if host (concat "@" host) "")
            (if suffix (concat ": " suffix) ""))))

(provide 'condorit-process)
;;; condorit-process.el ends here
