;;; condorit-history-test.el --- Tests for history and submit  -*- lexical-binding: t; -*-

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

(ert-deftest condorit-history-args ()
  (should (equal (condorit-history--user-args '("--mine" "-limit=100") "alice")
                 '("-limit" "100" "alice")))
  (should (equal (condorit-history--user-args '("-constraint=ExitCode != 0") "alice")
                 '("-constraint" "ExitCode != 0")))
  (should (equal (condorit-command-string
                  "condor_history" (condorit-history--user-args '("--mine" "-limit=100") "alice"))
                 "condor_history -limit 100 alice"))
  (let ((args (condorit-history--query-args '("--mine") "alice")))
    (should (equal (seq-take args 2) '("-json" "-attributes")))
    (should (equal (car (last args)) "alice"))))

(ert-deftest condorit-history-open ()
  (condorit-test-with-fake-condor
      `(("condor_history" :out ,(condorit-test-fixture "condor_history.json")))
    (condorit-history--open '("--mine" "-limit=100") "alice")
    (condorit-test-in-buffer "*condorit-history*"
                            (condorit-test-wait (lambda () (not condorit--pending)))
                            (should (string-match-p "^History: condor_history -limit 100 alice" (buffer-string)))
                            (should (string-match-p "^Finished jobs (3)" (buffer-string)))
                            (should (string-match-p "1201 +sim_batch +C 2  X 1" (buffer-string)))
                            (should (gethash "1201.1" condorit-jobs--by-id)))))

(ert-deftest condorit-history-refresh-failure-keeps-jobs ()
  ;; The history view shares the status view's refresh: a failure keeps
  ;; the jobs shown so far and says when and why it failed.
  (condorit-test-with-fake-condor
      `(("condor_history" :out ,(condorit-test-fixture "condor_history.json")))
    (condorit-history--open '("--mine") "alice")
    (with-current-buffer "*condorit-history*"
      (unwind-protect
          (progn
            (condorit-test-wait (lambda () (not condorit--pending)))
            (condorit-test-with-fake-condor
                '(("condor_history" :err "schedd down\n" :exit 1))
              (condorit-refresh)
              (condorit-test-wait (lambda () (not condorit--pending))))
            (should (string-match-p
                     "^⚠ refresh failed [0-9:]+: condor_history failed (exit 1): schedd down (see \\$)"
                     (buffer-string)))
            (should (string-match-p "^Finished jobs (3)" (buffer-string))))
        (kill-buffer)))))

(ert-deftest condorit-submit-args ()
  (should (equal (condorit-submit--args '("-dry-run" "-batch-name=foo") "t.sub")
                 '("-dry-run" "-" "-batch-name" "foo" "t.sub"))))

(ert-deftest condorit-submit-default-file ()
  (with-temp-buffer
    (setq buffer-file-name "/x/job.sub")
    (should (equal (condorit-submit--default-file) "/x/job.sub"))
    (setq buffer-file-name "/x/notes.txt")
    (should (null (condorit-submit--default-file)))
    (setq buffer-file-name nil)))

(ert-deftest condorit-submit-run-in-file-directory ()
  (condorit-test-with-fake-condor '(("condor_submit" :out "2 job(s) submitted to cluster 77.\n"))
    (let ((dir (make-temp-file "condorit-sub" t))
          done)
      (unwind-protect
          (cl-letf (((symbol-function 'condorit-refresh-all) (lambda (&rest _) (setq done t))))
            (condorit-submit--run nil (expand-file-name "job.sub" dir))
            (condorit-test-wait (lambda () done))
            (should (equal (condorit-test-argv) '(("condor_submit" "job.sub")))))
        (delete-directory dir t)))))

;;; condorit-history-test.el ends here
