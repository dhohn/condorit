;;; condorit-constraint-test.el --- Tests for constraint help  -*- lexical-binding: t; -*-

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

(ert-deftest condorit-constraint-unknown-attributes ()
  (should (equal (condorit-constraint--unknown
                  "Foo == 1 && regexp(\"Bar Baz\", Cmd) && TARGET.Memory > MY.Qux && x =?= undefined"
                  '("Cmd" "Memory"))
                 '("Foo" "Qux" "x")))
  ;; Case does not matter in ClassAds.
  (should-not (condorit-constraint--unknown "jobstatus == 5 && True" '("JobStatus"))))

(ert-deftest condorit-constraint-job-examples-from-point ()
  (condorit-test-with-status-buffer ()
    (condorit-test-goto 'condorit-job "1234.2")
    (let ((examples (condorit-constraint--job-examples)))
      (should (equal (seq-take examples 6)
                     '("ClusterId == 1234" "ClusterId == 1234 && ProcId == 2"
                       "JobBatchName == \"train_net\"" "Owner == \"alice\""
                       "JobStatus == 5" "RequestMemory >= 4096")))
      ;; General examples follow.
      (should (member "time() - QDate > 86400" examples)))
    ;; Not on a job: only the general ones.
    (goto-char (point-min))
    (should (equal (car (condorit-constraint--job-examples)) "JobStatus == 5"))))

(ert-deftest condorit-constraint-slot-examples-from-point ()
  (with-temp-buffer
    (condorit-pool-mode)
    (setq condorit--data (list :slots (condorit-classad-parse
                                      (condorit-test-fixture "condor_status.json"))
                              :time condorit-test-now))
    (condorit--render)
    (magit-section-show-level-4-all)
    (condorit-test-goto 'condorit-slot "slot1_1@wn01.example.org")
    (should (equal (seq-take (condorit-constraint--slot-examples) 2)
                   '("Machine == \"wn01.example.org\"" "Name == \"slot1_1@wn01.example.org\"")))))

(ert-deftest condorit-constraint-completion ()
  (condorit-test-with-status-buffer ()
    (let ((candidates (condorit-constraint--attributes 'jobs)))
      ;; Curated first, with docs; then what the view fetched.
      (should (equal (car candidates) '("ClusterId" . "cluster number")))
      (should (assoc "Iwd" candidates))
      (with-temp-buffer
        (setq condorit-constraint--candidates candidates)
        (insert "JobStatus == 5 && RequestMem")
        (pcase-let ((`(,start ,end ,table . ,props) (condorit-constraint--completion-at-point)))
          (should (equal (buffer-substring start end) "RequestMem"))
          (should (equal (all-completions "RequestMem" table) '("RequestMemory")))
          (should (equal (funcall (plist-get props :annotation-function) "RequestMemory")
                         "  memory requested, MiB")))
        ;; With nothing typed, after an operator: all attributes.
        (save-excursion
          (insert " && ")
          (pcase-let ((`(,start ,end ,table . ,_) (condorit-constraint--completion-at-point)))
            (should (= start end))
            (should (member "ClusterId" (all-completions "" table)))
            (delete-region (- (point) 4) (point))))
        ;; Not inside a string.
        (insert " && Cmd == \"Requ")
        (should-not (condorit-constraint--completion-at-point))))))

(ert-deftest condorit-constraint-check ()
  (condorit-test-with-fake-condor '(("condor_q" :out "1234\n1234\n1240\n"))
    (should (equal (condorit-constraint--check
                    '("condor_q" ("-allusers" "-af" "ClusterId") "jobs") "JobStatus == 1")
                   '(3 . "matches 3 jobs")))
    (should (equal (condorit-test-argv)
                   '(("condor_q" "-constraint" "JobStatus == 1" "-allusers" "-af" "ClusterId")))))
  (condorit-test-with-fake-condor
      '(("condor_q" :err "-- Parse error in constraint expression \"JobStatus ==\"\n" :exit 1))
    (pcase-let ((`(,ok . ,message) (condorit-constraint--check
                                    '("condor_q" nil "jobs") "JobStatus ==")))
      (should-not ok)
      (should (string-match-p "Parse error" message)))))

(defun condorit-constraint-test--read (inputs responses)
  "Run the queue reader, typing INPUTS in turn, with fake condor RESPONSES.
Return (RESULT PROMPTS DEFAULTS), the prompts and M-n lists it showed."
  (let (prompts defaults)
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (prompt _initial _keymap _read _history default)
                 (push prompt prompts)
                 (push default defaults)
                 (pop inputs))))
      (condorit-test-with-fake-condor responses
        (condorit-test-with-status-buffer ()
          (condorit-test-goto 'condorit-cluster "1240")
          (list (condorit-constraint-read-queue "Constraint: " nil nil)
                (nreverse prompts) (nreverse defaults)))))))

(ert-deftest condorit-constraint-read-checks-before-use ()
  ;; Accepted at once: the count goes to the echo area.
  (pcase-let ((`(,result ,_ ,defaults)
               (condorit-constraint-test--read '("ClusterId == 1240") '(("condor_q" :out "1240\n")))))
    (should (equal result "ClusterId == 1240"))
    (should (equal (car (car defaults)) "ClusterId == 1240")))
  ;; Rejected: offered again with condor's message; RET unchanged uses it.
  (pcase-let ((`(,result ,prompts ,_)
               (condorit-constraint-test--read
                '("JobStatus ==" "JobStatus ==")
                '(("condor_q" :err "Parse error in constraint\n" :exit 1)))))
    (should (equal result "JobStatus =="))
    (should (string-match-p "Parse error in constraint; RET again" (nth 1 prompts))))
  ;; Empty input clears the option, without asking condor.
  (should-not (car (condorit-constraint-test--read '("") nil))))

;;; condorit-constraint-test.el ends here
