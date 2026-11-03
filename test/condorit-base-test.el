;;; condorit-base-test.el --- Tests for the shared refresh  -*- lexical-binding: t; -*-

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

(ert-deftest condorit-base-parse-results ()
  (let ((data (condorit--parse-results
               condorit-test-userprio-spec
               `((jobs "condor_q" 0 ,(condorit-test-fixture "condor_q.json") "")
                 (finished "condor_history" 0 "" "")
                 (slots "condor_status" 0 "[]" "")
                 (userprio "condor_userprio" 0 ,(condorit-test-fixture "condor_userprio.long") ""))
               condorit-test-now)))
    (should (= (length (plist-get data :jobs)) 6))
    (should (null (plist-get data :finished)))
    (should (= (length (plist-get data :userprio)) 3))
    (should (= (plist-get data :time) condorit-test-now))))

(ert-deftest condorit-base-parse-results-errors ()
  (should (equal (cadr (should-error
                        (condorit--parse-results nil '((jobs "condor_q" 1 "" "schedd down")) 0)))
                 "condor_q failed (exit 1): schedd down"))
  (should (equal (cadr (should-error
                        (condorit--parse-results
                         nil '((jobs "condor_q" 0 "-- Failed to fetch ads" "")) 0)))
                 "condor_q printed output that is not JSON: -- Failed to fetch ads")))

;;;; A failing refresh start must not leave "Refreshing…" behind

(ert-deftest condorit-base-pool-refresh-start-error-redraws ()
  (with-temp-buffer
    (condorit-pool-mode)
    (let ((exec-path '("/nonexistent")))
      (should-error (condorit-refresh) :type 'user-error))
    (should-not condorit--pending)
    (should-not (string-match-p "Refreshing…" (buffer-string)))))

(ert-deftest condorit-base-history-refresh-start-error-redraws ()
  (with-temp-buffer
    (condorit-history-mode)
    (setq condorit--user "alice" condorit-options '("--mine"))
    (let ((exec-path '("/nonexistent")))
      (should-error (condorit-refresh) :type 'user-error))
    (should-not condorit--pending)
    (should-not (string-match-p "Refreshing…" (buffer-string)))))

;;; condorit-base-test.el ends here
