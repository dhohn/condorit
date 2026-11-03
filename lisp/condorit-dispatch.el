;;; condorit-dispatch.el --- The condorit dispatch menu  -*- lexical-binding: t; -*-

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
;; One menu listing every condorit command, so nothing has to be
;; memorized.  Bound to ? and h in all condorit buffers.

;;; Code:

(require 'transient)
(require 'condorit-faces)
(require 'condorit-base)
(require 'condorit-constraint)

(declare-function condorit-pool--condor-args "condorit-pool" (options))

(defun condorit-dispatch--legend ()
  "Return the job-state legend, with `face' properties as transient shows them."
  (condorit-state-legend 'face))

(defun condorit-dispatch--preview ()
  "Return the preview of the current view's command with the chosen options.
The pool view's own filters (\"--free-gpus\" etc.) never reach
condor_status; they follow the command, marked as condorit's."
  (let ((options (transient-get-value)))
    (cond ((derived-mode-p 'condorit-pool-mode)
           (let ((local (seq-filter (lambda (option) (string-prefix-p "--" option)) options)))
             (concat (condorit--preview "condor_status" (condorit-pool--condor-args options))
                     (if local
                         (propertize (format "   then in condorit: %s" (string-join local " "))
                                     'face 'transient-inactive-value)
                       ""))))
          ((derived-mode-p 'condorit-users-mode)
           (condorit--preview "condor_userprio" (condorit-expand-args options)))
          (t (condorit--preview "condor_q" (condorit-expand-args options))))))

(transient-define-suffix condorit-apply-options (args)
  "Refresh this view with the options set in this menu."
  (interactive (list (transient-args 'condorit-dispatch)))
  (condorit-set-options args))

;;;###autoload (autoload 'condorit-dispatch "condorit-dispatch" nil t)
(transient-define-prefix condorit-dispatch ()
  "Show all condorit commands."
  :init-value (lambda (obj)
                ;; The options and the man page `C-h' opens for them
                ;; belong to the main command of the current view.
                (oset obj man-page (cond ((derived-mode-p 'condorit-users-mode)
                                          "condor_userprio")
                                         ((derived-mode-p 'condorit-pool-mode)
                                          "condor_status")
                                         (t "condor_q")))
                (oset obj value (and (derived-mode-p 'condorit-status-mode 'condorit-users-mode
                                                     'condorit-pool-mode)
                                     condorit-options)))
  ;; One group per view: transient finds a suffix by its command, and
  ;; equal keys in one group (e.g. -a here and in the users view) would
  ;; share one, hidden or not.  Suffixes of hidden groups are left out.
  ["This view"
   :if-derived condorit-status-mode
   ("-a" "All users' jobs" "-allusers")
   ("-g" "Jobs of all schedds in the pool" "-global")
   ("-c" "Only jobs matching a ClassAd expression" "-constraint="
    :reader condorit-constraint-read-queue)
   (:info #'condorit-dispatch--preview)
   ("o" "Sort jobs…" condorit-jobs-sort)
   ("g" "Refresh with these options" condorit-apply-options)]
  ["This view"
   :if-derived condorit-history-mode
   ;; Its options are set in its own menu, `condorit-history'.
   ("o" "Sort jobs…" condorit-jobs-sort)]
  ["This view"
   :if-derived condorit-users-mode
   ("-a" "Include idle users" "-allusers")
   (:info #'condorit-dispatch--preview)
   ("g" "Refresh with these options" condorit-apply-options)]
  ["This view"
   :if-derived condorit-pool-mode
   ("-f" "Only machines with free CPUs" "--free-cpus")
   ("-G" "Only machines with free GPUs" "--free-gpus")
   ("-m" "Only machines running my jobs" "--mine")
   ("-c" "Only slots matching a ClassAd expression" "-constraint="
    :reader condorit-constraint-read-slots)
   ("-g" "Group machines by" "--group=" :choices ("gpu" "cpu" "os"))
   (:info #'condorit-dispatch--preview)
   ("g" "Refresh with these options" condorit-apply-options)]
  ["Legend"
   :if-derived condorit-jobs-mode
   (:info #'condorit-dispatch--legend)]
  [["Job(s) at point"
    :if-derived condorit-jobs-mode
    ("RET" "Show full ClassAd (condor_q -l)" condorit-visit)
    ("a" "Why isn't it running? (condor_q -better-analyze)" condorit-analyze
     :if-derived condorit-status-mode)
    ("k" "Remove (condor_rm)" condorit-remove :if-derived condorit-status-mode)
    ("H" "Hold (condor_hold)" condorit-hold :if-derived condorit-status-mode)
    ("R" "Release (condor_release)" condorit-release :if-derived condorit-status-mode)
    ("e" "Edit an attribute (condor_qedit)" condorit-edit :if-derived condorit-status-mode)
    ("S" "Shell in the job (condor_ssh_to_job)" condorit-ssh-to-job
     :if-derived condorit-status-mode)
    ("l" "Logs, stdout, stderr…" condorit-logs)]
   ["Machine or slot at point"
    :if-derived condorit-pool-mode
    ("RET" "Show full ClassAd (condor_status -l)" condorit-pool-visit)]
   ["Views"
    ("t" "History" condorit-history)
    ("P" "Pool" condorit-pool)
    ("U" "Users" condorit-users)
    ("$" "Command log" condorit-show-log)
    ("s" "Submit…" condorit-submit)]])

(provide 'condorit-dispatch)
;;; condorit-dispatch.el ends here
