;;; condorit-users.el --- The users view  -*- lexical-binding: t; -*-

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
;; The users view: the pool's submitters with their priorities, and the
;; schedds that have jobs.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'condorit-process)
(require 'condorit-classad)
(require 'condorit-faces)
(require 'condorit-base)

(defconst condorit-users--user-columns
  '(("USER" 36) ("PRIORITY" 10 right) ("IN USE" 6 right))
  "Columns of user rows.")

(defconst condorit-users--schedd-columns
  '(("SCHEDD" 32) ("RUNNING" 7 right) ("IDLE" 6 right) ("HELD" 6 right))
  "Columns of schedd rows.")

(defconst condorit-users--schedd-attributes
  '("Name" "TotalRunningJobs" "TotalIdleJobs" "TotalHeldJobs")
  "Schedd attributes condorit asks condor_status -schedd for.")

(defun condorit-users--commands ()
  "Return the commands a refresh of the users view runs."
  `((userprio "condor_userprio" ,(append condorit-options '("-long"))
              condorit-classad-parse-userprio)
    (schedds "condor_status"
             ("-schedd" "-json" "-attributes"
              ,(string-join condorit-users--schedd-attributes ",")))))

(defun condorit-users--users (ads)
  "Return the user ads of condor_userprio ADS, busiest first."
  (sort (seq-remove (lambda (ad) (eq (plist-get ad :IsAccountingGroup) t)) ads)
        (lambda (a b) (> (or (plist-get a :ResourcesUsed) 0)
                         (or (plist-get b :ResourcesUsed) 0)))))

(defun condorit-users--job-total (ad)
  "Return the running, idle and held jobs of schedd AD, added up."
  (+ (or (plist-get ad :TotalRunningJobs) 0)
     (or (plist-get ad :TotalIdleJobs) 0)
     (or (plist-get ad :TotalHeldJobs) 0)))

(defun condorit-users--schedds (ads)
  "Return the schedd ADS that have jobs, most running jobs first."
  (sort (seq-filter (lambda (ad) (> (condorit-users--job-total ad) 0)) ads)
        (lambda (a b) (> (or (plist-get a :TotalRunningJobs) 0)
                         (or (plist-get b :TotalRunningJobs) 0)))))

(defun condorit-users--count (n face)
  "Format job count N, in FACE when it is non-zero."
  (let ((text (number-to-string (or n 0))))
    (if (and n (> n 0)) (condorit--propertize text face) text)))

(defun condorit-users--insert (data)
  "Insert the users view's content from DATA."
  (let* ((users (condorit-users--users (plist-get data :userprio)))
         (all-schedds (plist-get data :schedds))
         (schedds (condorit-users--schedds all-schedds)))
    (insert (format "%s %s\n" (condorit--label "Users:") (condorit--refresh-state)))
    (condorit--insert-options)
    (condorit--insert-error)
    (insert "\n")
    (magit-insert-section (condorit-users)
      (magit-insert-heading (format "Users (%d)" (length users)))
      (insert (condorit--header-row "  " condorit-users--user-columns))
      (dolist (ad users)
        (magit-insert-section (condorit-user (plist-get ad :Name))
          (magit-insert-heading
            (condorit--row "  " condorit-users--user-columns
                          (list (or (plist-get ad :Name) "?")
                                (condorit--propertize
                                 (format "%.1f" (or (plist-get ad :Priority) 0))
                                 'condorit-number)
                                (condorit--number (or (plist-get ad :ResourcesUsed) 0)))))))
      (insert "\n"))
    (magit-insert-section (condorit-schedds)
      (magit-insert-heading (format "Schedds (%d of %d with jobs)"
                                    (length schedds) (length all-schedds)))
      (insert (condorit--header-row "  " condorit-users--schedd-columns))
      (dolist (ad schedds)
        (magit-insert-section (condorit-schedd (plist-get ad :Name))
          (magit-insert-heading
            (condorit--row "  " condorit-users--schedd-columns
                          (list (or (plist-get ad :Name) "?")
                                (condorit-users--count (plist-get ad :TotalRunningJobs)
                                                      'condorit-state-running)
                                (condorit-users--count (plist-get ad :TotalIdleJobs)
                                                      'condorit-state-idle)
                                (condorit-users--count (plist-get ad :TotalHeldJobs)
                                                      'condorit-state-held)))))))))

(define-derived-mode condorit-users-mode condorit-mode "Condorit-Users"
  "Major mode for the HTCondor users and schedds view."
  (setq-local condorit--fetch-function #'condorit-users--commands
              condorit--insert-function #'condorit-users--insert))

;;;###autoload
(defun condorit-users ()
  "Show the pool's submitters, their priorities, and the schedds with jobs."
  (interactive)
  (let ((buffer (condorit--view-buffer (condorit--buffer-name "condorit-users")
                                      #'condorit-users-mode)))
    (with-current-buffer buffer
      (condorit-refresh))
    (pop-to-buffer buffer)))

(provide 'condorit-users)
;;; condorit-users.el ends here
