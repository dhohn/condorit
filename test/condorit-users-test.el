;;; condorit-users-test.el --- Tests for the users view  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'condorit-test-helper)
(require 'condorit)

(defun condorit-test-schedds ()
  (condorit-classad-parse (condorit-test-fixture "condor_status_schedd.json")))

(ert-deftest condorit-users-users-sorted-without-groups ()
  (should (equal (mapcar (lambda (ad) (plist-get ad :Name))
                         (condorit-users--users
                          (condorit-classad-parse-long (condorit-test-fixture "condor_userprio.long"))))
                 '("bob@example.org" "alice@example.org"))))

(ert-deftest condorit-users-schedds-with-jobs-busiest-first ()
  (should (equal (mapcar (lambda (ad) (plist-get ad :Name))
                         (condorit-users--schedds (condorit-test-schedds)))
                 '("sched1.example.org" "sched2.example.org"))))

(ert-deftest condorit-users-render ()
  (with-temp-buffer
    (condorit-users-mode)
    (setq condorit--data
          (list :userprio (condorit-classad-parse-long (condorit-test-fixture "condor_userprio.long"))
                :schedds (condorit-test-schedds)
                :time condorit-test-now))
    (condorit--render)
    (let ((text (buffer-string)))
      (should (string-match-p "^Users (2)\n  USER +PRIORITY +IN USE\n" text))
      (should (string-match-p "bob@example.org +2250.0 +12" text))
      (should-not (string-match-p "<none>" text))
      (should (string-match-p "^Schedds (2 of 3 with jobs)\n  SCHEDD +RUNNING +IDLE +HELD\n" text))
      (should (string-match-p "sched1.example.org +900 +600 +0" text))
      (should (string-match-p "sched2.example.org +100 +2 +1" text))
      (should-not (string-match-p "login" text))
      (should (< (string-match "sched1" text) (string-match "sched2" text))))
    (goto-char (point-min))
    (re-search-forward "sched2.example.org +100 +2 +\\(1\\)")
    (should (eq (get-text-property (match-beginning 1) 'font-lock-face) 'condorit-state-held))))

(ert-deftest condorit-users-refresh-end-to-end ()
  (condorit-test-with-fake-condor
      `(("condor_status" :out ,(condorit-test-fixture "condor_status_schedd.json"))
        ("condor_userprio" :out ,(condorit-test-fixture "condor_userprio.long")))
    (with-temp-buffer
      (condorit-users-mode)
      (condorit-refresh)
      (condorit-test-wait (lambda () (not condorit--pending)))
      (should-not condorit--error)
      (should (string-match-p "^Users (2)" (buffer-string)))
      (should (string-match-p "^Schedds (2 of 3 with jobs)" (buffer-string)))
      (should (member '("condor_status" "-schedd" "-json" "-attributes"
                        "Name,TotalRunningJobs,TotalIdleJobs,TotalHeldJobs")
                      (condorit-test-argv))))))

(ert-deftest condorit-users-key-and-buffer ()
  (should (eq (keymap-lookup condorit-mode-map "U") 'condorit-users))
  (let ((default-directory temporary-file-directory))
    (should (equal (condorit--buffer-name "condorit-users") "*condorit-users*"))))

;;;; Including idle users

(defun condorit-test-users-responses ()
  `(("condor_status" :out ,(condorit-test-fixture "condor_status_schedd.json"))
    ("condor_userprio" :out ,(condorit-test-fixture "condor_userprio.long"))))

(ert-deftest condorit-users-active-users-by-default ()
  (condorit-test-with-fake-condor (condorit-test-users-responses)
    (with-temp-buffer
      (condorit-users-mode)
      (condorit-refresh)
      (condorit-test-wait (lambda () (not condorit--pending)))
      (should (member '("condor_userprio" "-long") (condorit-test-argv)))
      (should-not (string-match-p "^Options:" (buffer-string))))))

(ert-deftest condorit-users-allusers-option ()
  (condorit-test-with-fake-condor (condorit-test-users-responses)
    (with-temp-buffer
      (condorit-users-mode)
      (condorit-set-options '("-allusers"))
      (condorit-test-wait (lambda () (not condorit--pending)))
      (should (equal condorit-options '("-allusers")))
      (should (member '("condor_userprio" "-allusers" "-long") (condorit-test-argv)))
      (should (string-match-p "^Options: +-allusers" (buffer-string))))))

;;; condorit-users-test.el ends here
