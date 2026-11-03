;;; condorit-process-test.el --- Tests for condorit-process  -*- lexical-binding: t; -*-

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
(require 'condorit-process)

(ert-deftest condorit-process-command-string ()
  (should (equal (condorit-command-string "condor_rm" '("-forcex" "1234.0"))
                 "condor_rm -forcex 1234.0"))
  (should (equal (condorit-command-string "condor_qedit" '("-constraint" "Owner == \"x\""))
                 "condor_qedit -constraint 'Owner == \"x\"'"))
  (should (equal (condorit-command-string "condor_hold" '("-reason" "it's"))
                 "condor_hold -reason 'it'\\''s'")))

(ert-deftest condorit-process-expand-args ()
  (should (equal (condorit-expand-args '("-forcex" "-reason=too big" "-constraint=A==\"b\""))
                 '("-forcex" "-reason" "too big" "-constraint" "A==\"b\""))))

(ert-deftest condorit-process-error-summary ()
  (should (equal (condorit-error-summary "condor_rm" 1 "Job 9.0 not found\n" "")
                 "condor_rm failed (exit 1): Job 9.0 not found"))
  (should (equal (condorit-error-summary "condor_q" 1 "ignored" "ERROR: schedd down\nmore")
                 "condor_q failed (exit 1): ERROR: schedd down"))
  (should (equal (condorit-error-summary "x" 2 "" "") "x failed (exit 2): no output")))

(ert-deftest condorit-process-run-async ()
  (condorit-test-with-fake-condor '(("condor_q" :out "hello\n" :err "warn\n" :exit 3))
    (let (result)
      (condorit-run "condor_q" '("-json" "a b") (lambda (&rest r) (setq result r)))
      (condorit-test-wait (lambda () result))
      (should (equal result '(3 "hello\n" "warn\n")))
      (should (equal (condorit-test-argv) '(("condor_q" "-json" "a b")))))))

(ert-deftest condorit-process-run-logs ()
  (condorit-test-with-fake-condor '(("condor_q" :out "hello\n"))
    (when (get-buffer condorit-log-buffer-name)
      (kill-buffer condorit-log-buffer-name))
    (let (done)
      (condorit-run "condor_q" '("-json") (lambda (&rest _) (setq done t)))
      (condorit-test-wait (lambda () done))
      (with-current-buffer condorit-log-buffer-name
        (should (derived-mode-p 'condorit-log-mode))
        (should (string-match-p "\\$ condor_q -json +\\[exit 0\\]" (buffer-string)))
        (should (string-match-p "^  hello$" (buffer-string)))))))

(ert-deftest condorit-process-log-shortens-successful-output ()
  (let ((condorit-log-buffer-name " *condorit-log-test*")
        (condorit-log-max-lines 2)
        (lines "1\n2\n3\n4\n"))
    (unwind-protect
        (progn
          (condorit--log "condor_q" '("-json") 0 lines "")
          (condorit--log "condor_rm" '("1.0") 1 lines "")
          (with-current-buffer condorit-log-buffer-name
            (should (string-match-p "\\[exit 0\\]\n  1\n  2\n  … \\[2 more lines\\]\n"
                                    (buffer-string)))
            ;; Failures are kept whole.
            (should (string-match-p "\\[exit 1\\]\n  1\n  2\n  3\n  4\n\\'" (buffer-string)))))
      (kill-buffer condorit-log-buffer-name))))

(ert-deftest condorit-process-log-drops-oldest-entries ()
  (let ((condorit-log-buffer-name " *condorit-log-test*")
        (condorit-log-max-size 300))
    (unwind-protect
        (progn
          (dotimes (i 20)
            (condorit--log "condor_q" (list (format "run%d" i)) 0 "out\n" ""))
          (with-current-buffer condorit-log-buffer-name
            (should (<= (buffer-size) 300))
            (should (string-match-p "\\`[0-9:]\\{8\\} \\$ condor_q" (buffer-string)))
            (should-not (string-match-p "run0 " (buffer-string)))
            (should (string-match-p "run19 " (buffer-string)))))
      (kill-buffer condorit-log-buffer-name))))

(ert-deftest condorit-process-run-sync ()
  (condorit-test-with-fake-condor '(("condor_config_val" :out "submit.example.org\n" :err "note\n"))
    (should (equal (condorit-run-sync "condor_config_val" "FULL_HOSTNAME")
                   '(0 "submit.example.org\n" "note\n")))))

(ert-deftest condorit-process-missing-executable ()
  (should-error (condorit-run "condorit-no-such-program" nil #'ignore) :type 'user-error)
  (should-error (condorit-run-sync "condorit-no-such-program") :type 'user-error)
  (should-error (condorit-run-all '((a "condorit-no-such-program" nil)) #'ignore)
                :type 'user-error))

(ert-deftest condorit-process-run-all ()
  (condorit-test-with-fake-condor '(("condor_q" :out "[]") ("condor_status" :out "x" :exit 1))
    (let (results)
      (condorit-run-all '((jobs "condor_q" ("-json")) (slots "condor_status" ("-json")))
                       (lambda (r) (setq results r)))
      (condorit-test-wait (lambda () results))
      (should (equal (assq 'jobs results) '(jobs "condor_q" 0 "[]" "")))
      (should (equal (assq 'slots results) '(slots "condor_status" 1 "x" ""))))))

(ert-deftest condorit-process-run-all-empty ()
  (let ((called 'no))
    (condorit-run-all nil (lambda (r) (setq called r)))
    (should (null called))))

;;; condorit-process-test.el ends here
