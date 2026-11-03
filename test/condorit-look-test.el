;;; condorit-look-test.el --- Tests for faces, column headers and legend  -*- lexical-binding: t; -*-

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

(defun condorit-test-face-at (regexp)
  "Return the `font-lock-face' at group 1 (or the start) of REGEXP's first match."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp)
    (get-text-property (or (match-beginning 1) (match-beginning 0)) 'font-lock-face)))

(defun condorit-test-column (regexp)
  "Return the column where group 1 of REGEXP's first match starts."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp)
    (goto-char (match-beginning 1))
    (current-column)))

(ert-deftest condorit-look-row-and-header ()
  (let ((columns '(("JOB" 10) ("ST" 3) ("BATCH" 20) ("DETAIL" nil))))
    (should (equal (substring-no-properties (condorit--header-row "  " columns))
                   "  JOB        ST  BATCH                DETAIL\n"))
    (should (eq (get-text-property 2 'font-lock-face (condorit--header-row "  " columns))
                'condorit-column-header))
    (should (equal (substring-no-properties
                    (condorit--row "  " columns '("1234.2" "H" "train_net" "x")))
                   "  1234.2     H   train_net            x"))
    (should (equal (condorit--row "" '(("A" 4) ("B" nil)) '("abcdefg" "z")) "abc… z"))))

(ert-deftest condorit-look-status-faces ()
  (condorit-test-with-status-buffer ()
    (should (eq (condorit-test-face-at "^  1234\\.2 +train_net +\\(H\\)") 'condorit-state-held))
    (should (eq (condorit-test-face-at "^  \\(1234\\.2\\)") 'condorit-id))
    (should (eq (condorit-test-face-at "\\(Job has gone over\\)") 'condorit-hold-reason))
    (should (eq (condorit-test-face-at "train_net +\\(R 1\\)") 'condorit-state-running))
    (should (eq (condorit-test-face-at "R 1  \\(I 1\\)") 'condorit-state-idle))
    (should (eq (condorit-test-face-at "\\(2h13m\\)") 'condorit-number))
    (should (eq (condorit-test-face-at "\\(✗ 1 failed\\)") 'condorit-failed))
    (should (eq (condorit-test-face-at "^\\(Schedd:\\)") 'condorit-header-label))))

(ert-deftest condorit-look-status-column-headers ()
  (condorit-test-with-status-buffer ()
    (let ((text (buffer-string)))
      (should (string-match-p "^Held (2)\n  ID +BATCH +STATE +TIME / DETAIL\n" text))
      (should (string-match-p "^My jobs (6)\n  ID +BATCH +STATE +TIME / DETAIL\n" text))
      (should (string-match-p
               "^Recently finished (last 24h) (3)\n  ID +BATCH +STATE +TIME / DETAIL\n" text)))
    (should (= (condorit-test-column "^  ID +\\(BATCH\\)")
               (condorit-test-column "^  1234\\.2 +\\(train_net\\)")
               (condorit-test-column "^  1234 +\\(train_net\\)")))
    (should (= (condorit-test-column "^  ID +BATCH +\\(STATE\\)")
               (condorit-test-column "^  1234\\.2 +train_net +\\(H\\)")
               (condorit-test-column "^  1234 +train_net +\\(R 1\\)")))))

(ert-deftest condorit-look-expanded-cluster-shares-the-grid ()
  ;; Job rows under a cluster: id indented inside the ID column, batch
  ;; left blank (it is the cluster's), state and detail in the same columns.
  (condorit-test-with-status-buffer ()
    (magit-section-show (condorit-test-find-section 'condorit-cluster "1234"))
    (should (string-match-p "^    1234\\.0 +R +2h13m$" (buffer-string)))
    (should (= (condorit-test-column "^  ID +BATCH +\\(STATE\\)")
               (condorit-test-column "^    1234\\.0 +\\(R\\)")))
    (should (= (condorit-test-column "^  ID +BATCH +STATE +\\(TIME\\)")
               (condorit-test-column "^    1234\\.0 +R +\\(2h13m\\)")))))

(ert-deftest condorit-look-allusers-header-aligns-with-clusters ()
  (condorit-test-with-status-buffer ('("-allusers"))
    (should (= (condorit-test-column "^All jobs (6)\n +ID +BATCH +\\(STATE\\)")
               (condorit-test-column "^ +1300 +mc_gen +\\(R 1\\)")))))

(ert-deftest condorit-look-legend ()
  (condorit-test-with-status-buffer ()
    (should (string-match-p
             "^States: +R running  I idle  H held  > transferring output  S suspended  C completed  X removed$"
             (buffer-string)))
    (should (eq (condorit-test-face-at "^States: +\\(R\\)") 'condorit-state-running))
    (let ((condorit-show-legend nil))
      (condorit--render))
    (should-not (string-match-p "^States:" (buffer-string)))))

(ert-deftest condorit-look-legend-in-dispatch ()
  ;; Transient menus show `face', not `font-lock-face'.
  (should (string-match-p "H held" (condorit-dispatch--legend)))
  (should (eq (get-text-property 0 'face (condorit-dispatch--legend)) 'condorit-state-running))
  (should (string-match-p "R running" (condorit-test-dispatch-text 'condorit-status-mode)))
  (should-not (string-match-p "R running" (condorit-test-dispatch-text 'condorit-pool-mode))))

(ert-deftest condorit-look-history-column-headers ()
  (condorit-test-with-fake-condor
      `(("condor_history" :out ,(condorit-test-fixture "condor_history.json")))
    (condorit-history--open '("--mine") "alice")
    (condorit-test-in-buffer "*condorit-history*"
                            (condorit-test-wait (lambda () (not condorit--pending)))
                            (should (string-match-p "^Finished jobs (3)\n  ID +BATCH +STATE +TIME / DETAIL\n"
                                                    (buffer-string))))))

(ert-deftest condorit-look-long-nested-ids-fit ()
  ;; Busy schedds have 7-digit cluster ids and thousands of procs.
  (condorit-test-with-status-buffer
   (nil (list :jobs (list '(:ClusterId 1234567 :ProcId 1234 :JobStatus 1))
              :time condorit-test-now))
   (magit-section-show (condorit-test-find-section 'condorit-cluster "1234567"))
   (should (string-match-p "^    1234567\\.1234 +I$" (buffer-string)))))

;;; condorit-look-test.el ends here
