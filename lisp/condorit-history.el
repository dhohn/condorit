;;; condorit-history.el --- The history view of condorit  -*- lexical-binding: t; -*-

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
;; The history view: finished jobs from condor_history, chosen in a
;; transient menu.

;;; Code:

(require 'subr-x)
(require 'magit-section)
(require 'transient)
(require 'condorit-process)
(require 'condorit-classad)
(require 'condorit-faces)
(require 'condorit-base)
(require 'condorit-jobs)
(require 'condorit-constraint)

(defun condorit-history--user-args (args user)
  "Turn transient ARGS into condor_history words; \"--mine\" becomes USER."
  (append (condorit-expand-args (remove "--mine" args))
          (and (member "--mine" args) (list user))))

(defun condorit-history--query-args (args user)
  "Return the condor_history arguments for transient ARGS and USER."
  (append (list "-json" "-attributes" (condorit-jobs-attributes))
          (condorit-history--user-args args user)))

(defun condorit-history--commands ()
  "Return the commands a refresh of the history view runs."
  `((jobs "condor_history" ,(condorit-history--query-args condorit-options condorit--user))))

(defun condorit-history--insert (data)
  "Insert the history view's content from DATA."
  (let ((jobs (plist-get data :jobs)))
    (condorit-jobs--index jobs)
    (insert (format "History: %s   %s\n"
                    (condorit-command-string
                     "condor_history" (condorit-history--user-args condorit-options condorit--user))
                    (condorit--refresh-state)))
    (condorit-jobs--insert-sort-line)
    (condorit--insert-error)
    (insert "\n")
    (magit-insert-section (condorit-finished)
      (magit-insert-heading (format "Finished jobs (%d)" (length jobs)))
      (if jobs
          (progn (insert (condorit--header-row "  " (condorit-jobs--columns)))
                 (condorit-jobs--insert-clusters jobs (float-time) "  " t))
        (insert "  No matching jobs.\n")))))

(define-derived-mode condorit-history-mode condorit-jobs-mode "Condorit-History"
  "Major mode for browsing finished HTCondor jobs."
  (setq-local condorit--fetch-function #'condorit-history--commands
              condorit--insert-function #'condorit-history--insert))

(defun condorit-history--open (args user)
  "Show condor_history with transient ARGS for USER."
  (let ((buffer (condorit--view-buffer (condorit--buffer-name "condorit-history")
                                      #'condorit-history-mode)))
    (with-current-buffer buffer
      (setq condorit--user user
            condorit-options args)
      (condorit-refresh))
    (pop-to-buffer buffer)))

(transient-define-suffix condorit-history-show (args)
  "Show finished jobs matching the chosen options."
  (interactive (list (transient-args 'condorit-history)))
  (condorit-history--open args (transient-scope 'condorit-history)))

;;;###autoload (autoload 'condorit-history "condorit-history" nil t)
(transient-define-prefix condorit-history (user)
  "Browse finished jobs (condor_history)."
  :man-page "condor_history"
  :value '("--mine" "-limit=100")
  ["History"
   ("-m" "Only my jobs" "--mine")
   ("-l" "Show at most N jobs" "-limit=" :reader transient-read-number-N+)
   ("-s" "Stop scanning at a job id or when an expression is true" "-since=")
   ("-c" "Only jobs matching a ClassAd expression" "-constraint="
    :reader condorit-constraint-read-history)
   (:info (lambda ()
            (condorit--preview
             "condor_history"
             (condorit-history--user-args (transient-get-value)
                                         (transient-scope 'condorit-history)))))
   ("t" "Show history" condorit-history-show)]
  (interactive (list (or condorit--user (condorit--detect-user))))
  (transient-setup 'condorit-history nil nil :scope user))

(provide 'condorit-history)
;;; condorit-history.el ends here
