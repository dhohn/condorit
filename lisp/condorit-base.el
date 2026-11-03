;;; condorit-base.el --- Buffers and refresh plumbing for condorit  -*- lexical-binding: t; -*-

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
;; What every condorit view shares: the parent mode and its keys, the
;; buffer-local state, fetching a view's data and redrawing it, keeping
;; point on the same section across redraws, and running actions.
;;
;; A view is a major mode derived from `condorit-mode' that sets
;; `condorit--fetch-function' (which commands to run) and
;; `condorit--insert-function' (how to show their output);
;; `condorit-refresh' does the rest.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'condorit-process)
(require 'condorit-classad)
(require 'condorit-faces)

;;;; Options

(defgroup condorit nil
  "A Magit-like interface to HTCondor."
  :group 'tools
  :prefix "condorit-")

(defcustom condorit-show-legend t
  "Whether status buffers show a line explaining the job-state letters."
  :type 'boolean
  :group 'condorit)

;;;; Buffer state

(defvar-local condorit--host nil "Schedd host name this buffer shows.")
(defvar-local condorit--user nil "User name on the condor host.")

(defvar-local condorit-options nil
  "Extra options of this view's main command, e.g. (\"-allusers\").")

(defvar-local condorit--fetch-function nil
  "Function returning the commands a refresh of this view runs.
It returns a list of (KEY PROGRAM ARGS [PARSER]).  PARSER turns the
command's stdout into the value stored under :KEY in `condorit--data';
it defaults to parsing JSON.")

(defvar-local condorit--insert-function nil
  "Function inserting this view's content, called with `condorit--data'.")

(defvar-local condorit--data nil
  "Last successful fetch: a plist with one entry per command, plus :time.")
(defvar-local condorit--error nil "Message of the last failed refresh, or nil.")
(defvar-local condorit--pending nil "Non-nil while a refresh runs.")
(defvar-local condorit--stale nil
  "Non-nil when a refresh was requested while another was running.")

(defvar condorit-after-refresh-hook nil
  "Hook run in a condorit buffer after a refresh brought new data.
The data is in `condorit--data'; the buffer is already redrawn.")

(defun condorit--detect-user ()
  "Return the user name on `default-directory's host."
  (pcase-let ((`(,exit ,stdout ,stderr) (condorit-run-sync "id" "-un")))
    (if (eql exit 0)
        (string-trim stdout)
      (user-error "%s" (condorit-error-summary "id" exit stdout stderr)))))

(defun condorit--detect-schedd ()
  "Return the schedd host name for `default-directory's host."
  (or (cl-loop for variable in '("SCHEDD_HOST" "FULL_HOSTNAME")
               for (exit stdout) = (condorit-run-sync "condor_config_val" variable)
               when (and (eql exit 0) (not (string-blank-p stdout)))
               return (string-trim stdout))
      (file-remote-p default-directory 'host)
      "local"))

;;;; Point

(defun condorit--point-ident ()
  "Return the identity of the section at point, or nil."
  (when-let* (((bound-and-true-p magit-root-section))
              (section (magit-current-section)))
    (magit-section-ident section)))

(defun condorit--visible-ancestor (section)
  "Return the outermost folded ancestor of SECTION, or SECTION itself."
  (let ((visible section))
    (while (setq section (oref section parent))
      (when (oref section hidden)
        (setq visible section)))
    visible))

(defun condorit--restore-point (ident line)
  "Move to the section identified by IDENT, else to LINE.
If that section is inside a folded one, move to the folded one.
Also moves the point of every window showing this buffer."
  (let ((section (and ident (magit-get-section ident))))
    (goto-char (point-min))
    (if (and section (not (eq section magit-root-section)))
        (goto-char (oref (condorit--visible-ancestor section) start))
      (forward-line (1- line))))
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-point window (point))))

;;;; Mode

(declare-function condorit-dispatch "condorit-dispatch" ())
(declare-function condorit-submit "condorit-submit" (file))
(declare-function condorit-history "condorit-history" (user))
(declare-function condorit-pool "condorit-pool" ())
(declare-function condorit-users "condorit-users" ())

(defvar-keymap condorit-mode-map
  :parent magit-section-mode-map
  "g"   #'condorit-refresh
  "?"   #'condorit-dispatch
  "h"   #'condorit-dispatch
  "$"   #'condorit-show-log
  "s"   #'condorit-submit
  "t"   #'condorit-history
  "P"   #'condorit-pool
  "U"   #'condorit-users
  "q"   #'quit-window)

(define-derived-mode condorit-mode magit-section-mode "Condorit"
  "Parent mode of all condorit buffers."
  :interactive nil
  (setq-local truncate-lines t))

(defun condorit--view-buffer (name mode)
  "Return the buffer NAME in MODE, creating it for `default-directory's host."
  (let ((dir default-directory)
        (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p mode)
        (funcall mode)
        (setq default-directory dir)))
    buffer))

;;;; Refresh

(defun condorit--parse-json (program stdout)
  "Parse STDOUT, the JSON output of PROGRAM, into a list of plists."
  (condition-case nil
      (condorit-classad-parse stdout)
    (json-error
     (error "%s printed output that is not JSON: %s" program
            (truncate-string-to-width (car (split-string (string-trim stdout) "\n"))
                                      80 nil nil "…")))))

(defun condorit--parse-results (specs results now)
  "Turn `condorit-run-all' RESULTS of SPECS into a plist, or signal an error.
The plist has one entry per result key, plus :time NOW.  A command
that failed or printed something unparsable signals an error."
  (let ((data (list :time now)))
    (pcase-dolist (`(,key ,program ,exit ,stdout ,stderr) results)
      (unless (eql exit 0)
        (error "%s" (condorit-error-summary program exit stdout stderr)))
      (let ((parser (nth 3 (assq key specs))))
        (setq data (plist-put data (intern (format ":%s" key))
                              (if parser
                                  (funcall parser stdout)
                                (condorit--parse-json program stdout))))))
    data))

(defun condorit--receive (buffer data error)
  "Store the fetched DATA, or the ERROR message of a failed fetch, in BUFFER.
Then redraw BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq condorit--pending nil)
      (if error
          (progn
            (setq condorit--error
                  (format "refresh failed %s: %s" (format-time-string "%H:%M") error))
            (message "condorit: %s" error))
        (setq condorit--data data
              condorit--error nil))
      (condorit--render)
      (unless error
        (run-hooks 'condorit-after-refresh-hook))
      (when condorit--stale
        (setq condorit--stale nil)
        (condorit-refresh)))))

(defun condorit-refresh ()
  "Fetch fresh data for the current condorit buffer and redraw it."
  (interactive)
  (cond
   ((not condorit--fetch-function)
    (user-error "Nothing to refresh here"))
   ;; The running fetch may predate a change (e.g. a job action), so
   ;; fetch again once it finishes rather than dropping the request.
   (condorit--pending
    (setq condorit--stale t))
   (t
    (let ((buffer (current-buffer))
          (specs (funcall condorit--fetch-function))
          (now (float-time)))
      (setq condorit--pending t)
      (condorit--render)
      (condition-case err
          (condorit-run-all (mapcar (lambda (spec) (seq-take spec 3)) specs)
                           (lambda (results)
                             (let (data error)
                               (condition-case err
                                   (setq data (condorit--parse-results specs results now))
                                 (error (setq error (error-message-string err))))
                               (condorit--receive buffer data error))))
        (error
         (setq condorit--pending nil)
         (condorit--render)
         (signal (car err) (cdr err))))))))

(defun condorit-refresh-all (&optional remote)
  "Refresh every condorit buffer on the host given by REMOTE.
REMOTE is a `file-remote-p' prefix, or nil for the local host."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'condorit-mode)
                 condorit--fetch-function
                 (equal (file-remote-p default-directory) remote))
        (condorit-refresh)))))

(defun condorit-set-options (options)
  "Use OPTIONS for this view's main command and refresh."
  (setq condorit-options options)
  (condorit-refresh))

;;;; Render

(defun condorit--render ()
  "Redraw the current condorit buffer from `condorit--data'."
  (let ((ident (condorit--point-ident))
        (line (line-number-at-pos))
        (inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (condorit-root)
      (funcall condorit--insert-function condorit--data))
    ;; Inserting a section only records whether it is folded; showing
    ;; the root applies that to the text, as `magit-refresh' does.
    (magit-section-show magit-root-section)
    (condorit--restore-point ident line)))

(defun condorit--refresh-state ()
  "Return \"Refreshing…\", \"Refreshed HH:MM:SS\" or \"\" for this buffer."
  (cond (condorit--pending "Refreshing…")
        (condorit--data (format-time-string "Refreshed %H:%M:%S"
                                           (plist-get condorit--data :time)))
        (t "")))

(defun condorit--insert-options ()
  "Insert a line listing `condorit-options', if there are any."
  (when condorit-options
    (insert (format "%s %s\n" (condorit--label "Options:")
                    (string-join condorit-options " ")))))

(defun condorit--insert-error ()
  "Insert a warning line for the last failed refresh, if there was one."
  (when condorit--error
    (insert (condorit--propertize (format "⚠ %s (see $)\n" condorit--error) 'warning))))

;;;; Actions

(defun condorit--preview (program args)
  "Return the preview line of PROGRAM with ARGS for transient menus."
  (propertize (concat "→ " (condorit-command-string program args)) 'face 'bold))

(defun condorit--run-action (program args)
  "Run PROGRAM with ARGS, report the result and refresh condorit buffers."
  (let ((remote (file-remote-p default-directory)))
    (message "Running %s…" (condorit-command-string program args))
    (condorit-run program args
                 (lambda (exit stdout stderr)
                   (if (eql exit 0)
                       (progn (message "%s" (string-trim stdout))
                              (condorit-refresh-all remote))
                     (message "%s" (condorit-error-summary program exit stdout stderr)))))))

(defun condorit--show-output (buffer-name program args text)
  "Show TEXT, the output of PROGRAM with ARGS, in a buffer named BUFFER-NAME."
  (let ((buffer (get-buffer-create buffer-name))
        (dir default-directory))
    (with-current-buffer buffer
      (special-mode)
      (setq default-directory dir)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (concat "$ " (condorit-command-string program args)) 'font-lock-face 'bold)
                "\n\n" text))
      (goto-char (point-min)))
    (display-buffer buffer)))

;;;; ClassAds

(define-derived-mode condorit-classad-mode special-mode "Condorit-ClassAd"
  "Major mode showing one full ClassAd, of a job or a slot."
  (setq-local font-lock-defaults
              '((("^\\([A-Za-z0-9_]+\\) =" 1 font-lock-variable-name-face)))))

(defun condorit--fetch-ad (program args)
  "Run PROGRAM with ARGS and return the first ad of its JSON output, or nil."
  (pcase-let ((`(,exit ,stdout ,stderr) (apply #'condorit-run-sync program args)))
    (unless (eql exit 0)
      (user-error "%s" (condorit-error-summary program exit stdout stderr)))
    (car (condorit-classad-parse stdout))))

(defun condorit--show-ad (buffer-name ad)
  "Show AD's attributes in a buffer named BUFFER-NAME."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (condorit-classad-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (condorit-classad-format-ad ad)))
      (goto-char (point-min)))
    (display-buffer buffer)))

(provide 'condorit-base)
;;; condorit-base.el ends here
