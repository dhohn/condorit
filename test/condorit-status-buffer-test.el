;;; condorit-status-buffer-test.el --- Tests for the status buffer  -*- lexical-binding: t; -*-

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

(ert-deftest condorit-status-render-sections ()
  (condorit-test-with-status-buffer ()
    (let ((text (buffer-string)))
      (should (string-match-p "^Schedd: +submit.example.org +User: alice" text))
      (should (string-match-p
               "4/96 CPUs busy · 2 machine(s) · 1 slot(s) claimed · 2 of your jobs idle on this schedd · priority 500.0 (lower is better)"
               text))
      (should (string-match-p "^Held (2)" text))
      (should (string-match-p "1234.2 +train_net +H +Job has gone over memory limit" text))
      (should (string-match-p "^My jobs (6)" text))
      (should (string-match-p "1234 +train_net +R 1  I 1  H 1 +2h13m$" text))
      (should (string-match-p "1240 +plots.py +I 1" text))
      (should (string-match-p "^Recently finished (last 24h) (3)" text))
      (should (string-match-p "1201 +sim_batch +C 2  X 1 +✗ 1 failed" text)))
    (should (oref (condorit-test-find-section 'condorit-cluster "1234") hidden))
    (should-not (oref (condorit-test-find-section 'condorit-held nil) hidden))
    (should (oref (condorit-test-find-section 'condorit-finished nil) hidden))
    (should (equal (plist-get (gethash "1201.1" condorit-jobs--by-id) :ExitCode) 2))))

(ert-deftest condorit-status-render-allusers ()
  (condorit-test-with-status-buffer ('("-allusers"))
    (should (string-match-p "^All jobs (6)" (buffer-string)))
    (should (string-match-p "^  bob (1)" (buffer-string)))
    (should (string-match-p "^Options: +-allusers" (buffer-string)))
    (should (condorit-test-find-section 'condorit-owner "alice"))))

(ert-deftest condorit-status-render-minimal-job ()
  (condorit-test-with-status-buffer
   (nil (list :jobs (list '(:ClusterId 7 :ProcId 0 :JobStatus 2)
                          '(:ClusterId 8 :ProcId 0 :JobStatus 5))
              :time condorit-test-now))
   (should (string-match-p "7 +\\? +R 1" (buffer-string)))
   (should (string-match-p "no hold reason given" (buffer-string)))
   (should (string-match-p "Nothing finished recently" (buffer-string)))))

(ert-deftest condorit-status-render-empty-queue ()
  (condorit-test-with-status-buffer (nil (list :time condorit-test-now))
    (should (string-match-p "^My jobs (0)" (buffer-string)))
    (should (string-match-p "No jobs in the queue" (buffer-string)))
    (should-not (string-match-p "^Held" (buffer-string)))))

(ert-deftest condorit-status-render-keeps-point-and-folds ()
  (condorit-test-with-status-buffer ()
    (magit-section-show (condorit-test-find-section 'condorit-cluster "1234"))
    (condorit-test-goto 'condorit-cluster "1240")
    (condorit--render)
    (should (equal (oref (magit-current-section) value) "1240"))
    (should-not (oref (condorit-test-find-section 'condorit-cluster "1234") hidden))))

(ert-deftest condorit-status-refresh-failure-keeps-data ()
  (condorit-test-with-status-buffer ()
    (setq condorit--pending t)
    (condorit--receive (current-buffer) nil "condor_q failed (exit 1): schedd down")
    (should-not condorit--pending)
    (should (string-match-p
             "^⚠ refresh failed [0-9:]+: condor_q failed (exit 1): schedd down (see \\$)"
             (buffer-string)))
    (should (string-match-p "^Held (2)" (buffer-string)))))

(ert-deftest condorit-status-receive-dead-buffer ()
  (let ((buffer (condorit-test-status-buffer)))
    (kill-buffer buffer)
    (condorit--receive buffer nil "x")))

(ert-deftest condorit-status-refresh-end-to-end ()
  (condorit-test-with-fake-condor (condorit-test-fixture-responses)
    (with-temp-buffer
      (condorit-status-mode)
      (setq condorit--host "submit.example.org" condorit--user "alice")
      (condorit-refresh)
      (should (string-match-p "Refreshing…" (buffer-string)))
      (condorit-test-wait (lambda () (not condorit--pending)))
      (should (string-match-p "^Held (2)" (buffer-string)))
      (should (string-match-p "Refreshed [0-9:]+" (buffer-string))))))

(ert-deftest condorit-status-auto-refresh-skips-when-pending ()
  (let ((calls 0)
        (buffer (condorit-test-status-buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'condorit-run-all) (lambda (&rest _) (cl-incf calls))))
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer (setq condorit--pending t))
          (condorit-status--auto-refresh buffer)
          (should (= calls 0))
          (with-current-buffer buffer (setq condorit--pending nil))
          (condorit-status--auto-refresh buffer)
          (should (= calls 1)))
      (kill-buffer buffer))))

(ert-deftest condorit-status-detect-schedd ()
  (condorit-test-with-fake-condor '(("condor_config_val" :out "submit.example.org\n"))
    (should (equal (condorit--detect-schedd) "submit.example.org")))
  (condorit-test-with-fake-condor '(("condor_config_val" :err "Not defined\n" :exit 1))
    (should (equal (condorit--detect-schedd) "local"))))

(ert-deftest condorit-status-entry ()
  (condorit-test-with-fake-condor
      (cons '("condor_config_val" :out "submit.example.org\n") (condorit-test-fixture-responses))
    (cl-letf (((symbol-function 'condorit--detect-user) (lambda () "alice")))
      (condorit-status)
      (condorit-test-in-buffer "*condorit*"
        (should (derived-mode-p 'condorit-status-mode))
        (condorit-test-wait (lambda () (not condorit--pending)))))))

(defun condorit-test-big-cluster (n)
  (cl-loop for proc below n
           collect (list :ClusterId 500 :ProcId proc :JobStatus 1 :Owner "alice"
                         :JobBatchName "big")))

(ert-deftest condorit-status-collapsed-cluster-body-is-lazy ()
  ;; Procs of a collapsed cluster are only inserted when it is expanded.
  (condorit-test-with-status-buffer ()
    (should-not (condorit-test-find-section 'condorit-job "1240.0"))
    (magit-section-show (condorit-test-find-section 'condorit-cluster "1240"))
    (should (condorit-test-find-section 'condorit-job "1240.0"))))

(ert-deftest condorit-status-render-large-cluster-fast ()
  (let ((start (float-time)))
    (condorit-test-with-status-buffer
        (nil (list :jobs (condorit-test-big-cluster 8000) :time condorit-test-now))
      (should (< (- (float-time) start) 1.0)))))

(ert-deftest condorit-status-refresh-while-pending-reruns ()
  ;; A refresh requested while one is running (e.g. after an action)
  ;; must not be lost: it runs once the current one finishes.
  (let ((calls 0))
    (cl-letf (((symbol-function 'condorit-run-all) (lambda (&rest _) (cl-incf calls))))
      (condorit-test-with-status-buffer ()
        (condorit-refresh)
        (should (= calls 1))
        (condorit-refresh)
        (should (= calls 1))
        (condorit--receive (current-buffer) (condorit-test-data) nil)
        (should (= calls 2))
        (should condorit--pending)
        (condorit--receive (current-buffer) (condorit-test-data) nil)
        (should (= calls 2))
        (should-not condorit--pending)))))

(ert-deftest condorit-status-recently-finished-starts-folded ()
  ;; Folded on screen, not only marked as hidden.
  (condorit-test-with-status-buffer ()
    (let ((finished (condorit-test-find-section 'condorit-finished nil)))
      (should (condorit-test-folded-p finished))
      (magit-section-show finished)
      (should-not (condorit-test-folded-p finished))
      ;; A redraw keeps it the way the user left it.
      (condorit--render)
      (should-not (condorit-test-folded-p (condorit-test-find-section 'condorit-finished nil))))))

(ert-deftest condorit-status-auto-refresh-waits-for-region ()
  ;; A redraw would move the mark of a region being selected.
  (let ((calls 0))
    (cl-letf (((symbol-function 'condorit-run-all) (lambda (&rest _) (cl-incf calls))))
      (condorit-test-with-status-buffer ()
        (set-window-buffer (selected-window) (current-buffer))
        (transient-mark-mode 1)
        (push-mark (point) t t)
        (forward-line 3)
        (condorit-status--auto-refresh (current-buffer))
        (should (= calls 0))
        (deactivate-mark)
        (condorit-status--auto-refresh (current-buffer))
        (should (= calls 1))))))

(ert-deftest condorit-status-history-limit-in-heading ()
  (let ((condorit-history-limit 3))
    (condorit-test-with-status-buffer ()
      (should (string-match-p "^Recently finished (last 24h) (3, the newest only: see condorit-history-limit)"
                              (buffer-string)))))
  (condorit-test-with-status-buffer ()
    (should (string-match-p "^Recently finished (last 24h) (3)$" (buffer-string)))))

(ert-deftest condorit-status-extra-job-columns ()
  (let ((condorit-job-columns '(("RequestMemory" "REQMEM" 6 right) ("ProcId" "PROC" 4 right)
                               ("Missing" "MISS" 4))))
    ;; The attributes are fetched once, after the built-in ones.
    (should (string-suffix-p ",Missing" (condorit-jobs-attributes)))
    (should (= (cl-count "RequestMemory" (split-string (condorit-jobs-attributes) ",")
                         :test #'equal)
               1))
    (condorit-test-with-status-buffer ()
      (magit-section-show-level-4-all)
      (let ((text (buffer-string)))
        (should (string-match-p "STATE +REQMEM PROC MISS TIME / DETAIL$" text))
        ;; A cluster shows a shared value, and "…" where its jobs differ.
        (should (string-match-p "^  1234 +train_net +R 1  I 1  H 1 +4096    … +2h13m$" text))
        (should (string-match-p "^    1234.1 +I +4096    1 +" text))))))

(ert-deftest condorit-status-extra-job-column-format ()
  (let ((condorit-job-columns '(("RemoteWallClockTime" "WALL" 6 right
                                condorit-classad-format-duration))))
    (should (equal (condorit-jobs--value '(:RemoteWallClockTime 7499.0) (car condorit-job-columns))
                   "2h04m"))
    ;; The format also gets nil, for jobs without the attribute.
    (should (equal (condorit-jobs--value '(:ClusterId 1) (car condorit-job-columns)) ""))))

(ert-deftest condorit-status-sorted-header-and-held ()
  (condorit-test-with-status-buffer ()
    (should-not (string-match-p "^Sorted:" (buffer-string)))
    (condorit-jobs-sort "name")
    (condorit-jobs-sort "name")
    (should (string-match-p "^Sorted: +name ↓$" (buffer-string)))))

(ert-deftest condorit-status-render-global ()
  ;; Both schedds have a cluster 1234.
  (condorit-test-with-status-buffer
      ('("-global" "-allusers")
       (list :jobs (condorit-classad-mark-schedds
                    '((:ClusterId 1234 :ProcId 0 :JobStatus 2 :Owner "alice"
                       :GlobalJobId "a.example.org#1234.0#1791960000")
                      (:ClusterId 1234 :ProcId 0 :JobStatus 1 :Owner "bob"
                       :GlobalJobId "b.example.org#1234.0#1791960000")))
             :time condorit-test-now))
    (should (string-match-p "^All jobs, all schedds (2)" (buffer-string)))
    (should (string-match-p "^  a.example.org (1)\n    alice (1)\n      1234 " (buffer-string)))
    (should (condorit-test-find-section 'condorit-cluster "a.example.org#1234"))
    (condorit-test-goto 'condorit-cluster "b.example.org#1234")
    (should (string-match-p "1 idle.*(schedd b.example.org)\\'" (condorit-jobs--eldoc #'ignore)))
    (magit-section-show (magit-current-section))
    (condorit-test-goto 'condorit-job "b.example.org#1234.0")
    (should (equal (condorit-jobs-at-point) '("b.example.org#1234.0")))))

(ert-deftest condorit-status-refresh-global ()
  (condorit-test-with-fake-condor
      (cons '("condor_q" :out "[{\"ClusterId\": 5, \"ProcId\": 0, \"JobStatus\": 1, \"Owner\": \"alice\", \"GlobalJobId\": \"b.example.org#5.0#1791960000\"}]")
            (cdr (condorit-test-fixture-responses)))
    (with-temp-buffer
      (condorit-status-mode)
      (setq condorit--host "submit.example.org" condorit--user "alice"
            condorit-options '("-global"))
      (condorit-refresh)
      (condorit-test-wait (lambda () (not condorit--pending)))
      (should (member "-global" (assoc "condor_q" (condorit-test-argv))))
      (should (condorit-test-find-section 'condorit-cluster "b.example.org#5")))))

(ert-deftest condorit-status-header-says-scope ()
  ;; The idle count covers only the jobs the options ask for.
  (condorit-test-with-status-buffer ('("-allusers" "-global"))
    (should (string-match-p "· 2 job(s) idle in the pool ·" (buffer-string)))
    (should (string-match-p "^Recently finished (last 24h, this schedd only)" (buffer-string)))))

(ert-deftest condorit-status-action-prompts ()
  (condorit-test-with-status-buffer ()
    (should (equal (condorit-jobs--describe-ids '("1234")) "1234 (3 jobs, 1 running)"))
    (should (equal (condorit-jobs--describe-ids '("1240.0" "1300"))
                   "1240.0 1300 (2 jobs, 1 running, of bob)"))
    (should (equal (condorit-jobs--describe-ids '("9")) "9"))
    (let (prompts)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (push prompt prompts) nil))
                ((symbol-function 'condorit--run-action) #'ignore))
        (condorit-actions--remove nil '("1234"))
        (should (equal prompts '("Remove 1234 (3 jobs, 1 running)? ")))))))

(ert-deftest condorit-status-action-menus-count-jobs ()
  ;; The menus are drawn in their own buffer; the counts come from the view's.
  (condorit-test-with-status-buffer ()
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (condorit-test-goto 'condorit-cluster "1234")
      (dolist (menu '(condorit-hold condorit-remove))
        (unwind-protect
            (progn (call-interactively menu)
                   (transient--redisplay)
                   (should (string-match-p "^\\(Hold\\|Remove\\) 1234 (3 jobs, 1 running)$"
                                           (with-current-buffer " *transient*" (buffer-string)))))
          (when transient--prefix (transient-quit-all)))))
    (let (prompt)
      (cl-letf (((symbol-function 'completing-read) (lambda (p &rest _) (setq prompt p) "JobPrio"))
                ((symbol-function 'read-string) (lambda (&rest _) "5"))
                ((symbol-function 'condorit--run-action) #'ignore))
        (condorit-test-goto 'condorit-cluster "1234")
        (call-interactively #'condorit-edit))
      (should (equal prompt "Attribute for 1234 (3 jobs, 1 running): ")))))

;;; condorit-status-buffer-test.el ends here
