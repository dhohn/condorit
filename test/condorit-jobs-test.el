;;; condorit-jobs-test.el --- Tests for condorit job actions  -*- lexical-binding: t; -*-

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

(ert-deftest condorit-jobs-normalize-ids ()
  (should (equal (condorit-jobs--normalize-ids '("1234" "1234.0" "1240.0" "1240.0"))
                 '("1234" "1240.0"))))

(ert-deftest condorit-jobs-ids-constraint ()
  (should (equal (condorit-jobs--ids-constraint '("1234.0" "1240"))
                 "(ClusterId == 1234 && ProcId == 0) || ClusterId == 1240")))

(ert-deftest condorit-jobs-arg-builders ()
  (should (equal (condorit-command-string "condor_rm" (condorit-actions--args '("-forcex") '("1234.0" "1240")))
                 "condor_rm -forcex 1234.0 1240"))
  (should (equal (condorit-actions--args '("-reason=too big") '("1234"))
                 '("-reason" "too big" "1234")))
  (should (equal (condorit-actions--qedit-args '("1234.0" "1240") "RequestMemory" "8192")
                 '("-constraint" "(ClusterId == 1234 && ProcId == 0) || ClusterId == 1240"
                   "RequestMemory" "8192"))))

(ert-deftest condorit-jobs-at-point ()
  (condorit-test-with-status-buffer ()
    (condorit-test-goto 'condorit-cluster "1234")
    (should (equal (condorit-jobs-at-point) '("1234")))
    (should (equal (condorit-classad-job-id (condorit-job-at-point)) "1234.0"))
    (magit-section-show (condorit-test-find-section 'condorit-cluster "1240"))
    (condorit-test-goto 'condorit-job "1240.0")
    (should (equal (condorit-jobs-at-point) '("1240.0")))
    (goto-char (point-min))
    (should-error (condorit-jobs-at-point) :type 'user-error)))

(ert-deftest condorit-jobs-at-point-region ()
  (condorit-test-with-status-buffer ()
    (transient-mark-mode 1)
    (condorit-test-goto 'condorit-cluster "1234")
    (push-mark (point) t t)
    (condorit-test-goto 'condorit-cluster "1241")
    (should (equal (condorit-jobs-at-point) '("1234" "1240" "1241")))
    (deactivate-mark)))

(ert-deftest condorit-jobs-job-file ()
  (let ((default-directory "/tmp/"))
    (should (equal (condorit-actions--job-file
                    '(:ClusterId 1 :ProcId 0 :Iwd "/home/a/w" :Out "out/1.out") :Out)
                   "/home/a/w/out/1.out"))
    (should (equal (condorit-actions--job-file
                    '(:ClusterId 1 :ProcId 0 :Iwd "/home/a/w" :UserLog "/var/log/j.log") :UserLog)
                   "/var/log/j.log"))
    (should-error (condorit-actions--job-file '(:ClusterId 1 :ProcId 0 :Out "/dev/null") :Out)
                  :type 'user-error)
    (should-error (condorit-actions--job-file '(:ClusterId 1 :ProcId 0) :Err)
                  :type 'user-error)
    (should-error (condorit-actions--job-file '(:ClusterId 1 :ProcId 0 :Out "rel.out") :Out)
                  :type 'user-error))
  (let ((default-directory "/ssh:submit:/home/a/"))
    (should (equal (condorit-actions--job-file
                    '(:ClusterId 1 :ProcId 0 :Iwd "/home/a/w" :Out "o.out") :Out)
                   "/ssh:submit:/home/a/w/o.out"))))

(ert-deftest condorit-jobs-open-file-quits-with-q ()
  (let* ((dir (make-temp-file "condorit-out" t))
         (file (expand-file-name "1.out" dir))
         (job (list :ClusterId 1 :ProcId 0 :Iwd dir :Out "1.out")))
    (unwind-protect
        (save-window-excursion
          (with-temp-file file (insert "hello\n"))
          (condorit-actions--open-file job :Out)
          (let ((buffer (current-buffer)))
            (should (equal buffer-file-name file))
            (should view-mode)
            (should buffer-read-only)
            (should (eq (key-binding "q") 'View-quit))
            (View-quit)
            (should-not (buffer-live-p buffer)))
          ;; A buffer that was open before stays open.
          (let ((buffer (find-file-noselect file)))
            (condorit-actions--open-file job :Out)
            (View-quit)
            (should (buffer-live-p buffer))
            (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest condorit-jobs-release-runs-and-refreshes ()
  (condorit-test-with-fake-condor '(("condor_release" :out "Job 1234.2 released\n"))
    (let (refreshed)
      (cl-letf (((symbol-function 'condorit-refresh-all) (lambda (&rest _) (setq refreshed t))))
        (condorit-release '("1234.2"))
        (condorit-test-wait (lambda () refreshed))
        (should (equal (condorit-test-argv) '(("condor_release" "1234.2"))))))))

(ert-deftest condorit-jobs-action-failure-does-not-refresh ()
  (condorit-test-with-fake-condor '(("condor_release" :out "Job 9.0 not found\n" :exit 1))
    (let (refreshed messages)
      (cl-letf (((symbol-function 'condorit-refresh-all) (lambda (&rest _) (setq refreshed t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (when fmt (push (apply #'format fmt args) messages)))))
        (condorit-release '("9.0"))
        (condorit-test-wait
         (lambda () (member "condor_release failed (exit 1): Job 9.0 not found" messages)))
        (should-not refreshed)))))

(ert-deftest condorit-jobs-remove-confirms ()
  (condorit-test-with-fake-condor '(("condor_rm" :out "ok\n"))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil))
              ((symbol-function 'condorit-refresh-all) #'ignore))
      (condorit-actions--remove '("-forcex") '("1234.0"))
      (accept-process-output nil 0.2)
      (should (null (condorit-test-argv))))
    (let (done)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'condorit-refresh-all) (lambda (&rest _) (setq done t))))
        (condorit-actions--remove '("-forcex") '("1234.0"))
        (condorit-test-wait (lambda () done))
        (should (equal (condorit-test-argv) '(("condor_rm" "-forcex" "1234.0"))))))))

(ert-deftest condorit-jobs-edit ()
  (condorit-test-with-fake-condor '(("condor_qedit" :out "Set attribute \"RequestMemory\".\n"))
    (let (done)
      (cl-letf (((symbol-function 'condorit-refresh-all) (lambda (&rest _) (setq done t))))
        (condorit-edit '("1234.2") "RequestMemory" "8192")
        (condorit-test-wait (lambda () done))
        (should (equal (condorit-test-argv)
                       '(("condor_qedit" "-constraint" "(ClusterId == 1234 && ProcId == 2)"
                          "RequestMemory" "8192"))))))))

(ert-deftest condorit-jobs-show-classad-falls-back-to-history ()
  (condorit-test-with-fake-condor
      `(("condor_q" :out "")
        ("condor_history" :out ,(condorit-test-fixture "condor_history.json")))
    (condorit-show-classad "1201.0")
    (condorit-test-in-buffer "*condorit-classad: 1201.0*"
                            (should (string-match-p "^JobBatchName = \"sim_batch\"$" (buffer-string))))
    (should (equal (mapcar #'car (condorit-test-argv)) '("condor_q" "condor_history")))))

(ert-deftest condorit-jobs-show-classad-missing ()
  (condorit-test-with-fake-condor '(("condor_q" :out "") ("condor_history" :out ""))
    (should-error (condorit-show-classad "9.0") :type 'user-error)))

(ert-deftest condorit-jobs-analyze ()
  (when (get-buffer "*condorit-analyze: 1234.1*")
    (kill-buffer "*condorit-analyze: 1234.1*"))
  (condorit-test-with-fake-condor
      '(("condor_q" :out "The Requirements expression for job 1234.1 is ...\n"))
    (condorit-analyze "1234.1")
    (condorit-test-wait (lambda () (get-buffer "*condorit-analyze: 1234.1*")))
    (condorit-test-in-buffer "*condorit-analyze: 1234.1*"
                            (should (string-match-p "^\\$ condor_q -better-analyze 1234.1" (buffer-string)))
                            (should (string-match-p "Requirements expression" (buffer-string))))))
(ert-deftest condorit-jobs-hold-reasons ()
  (should (string-match-p "more of a resource" (car (condorit-jobs--hold-explanation 34))))
  (should (string-match-p "release with \\\\\\[condorit-release\\]"
                          (cadr (condorit-jobs--hold-explanation 34))))
  (should (null (condorit-jobs--hold-explanation 9999)))
  (should (null (condorit-jobs--hold-explanation nil))))

(ert-deftest condorit-jobs-explain ()
  ;; Hints name the keys of the current buffer.
  (with-temp-buffer
    (use-local-map condorit-status-mode-map)
    (should (equal (condorit-jobs--explain '(:JobStatus 5 :HoldReasonCode 34) condorit-test-now)
                   "Held: The job used more of a resource (usually memory) than it requested. Raise the request (e.g. RequestMemory) with e, then release with R."))
    (should (equal (condorit-jobs--explain '(:JobStatus 5 :HoldReason "weird") condorit-test-now)
                   "Held: weird  Fix the cause, then release with R."))
    (should (string-match-p "Press a" (condorit-jobs--explain '(:JobStatus 1) condorit-test-now)))
    (should (equal (condorit-jobs--explain '(:JobStatus 2 :JobCurrentStartDate 1791962000
                                            :RemoteHost "slot1@wn01")
                                          condorit-test-now)
                   "Running for 2h13m on slot1@wn01."))
    (should (string-match-p "exit code 2" (condorit-jobs--explain '(:JobStatus 4 :ExitCode 2) condorit-test-now)))
    (should (null (condorit-jobs--explain '(:JobStatus 99) condorit-test-now)))))

(ert-deftest condorit-jobs-eldoc-at-point ()
  (condorit-test-with-status-buffer ()
    (should (memq #'condorit-jobs--eldoc eldoc-documentation-functions))
    (condorit-test-goto 'condorit-job "1234.2")
    (should (string-prefix-p "Held: The job used more" (condorit-jobs--eldoc #'ignore)))
    ;; A cluster sums up its jobs.
    (condorit-test-goto 'condorit-cluster "1234")
    ;; Eldoc measures against the clock, not the fixtures' time.
    (should (string-match-p
             (concat "\\`Cluster 1234 (train_net): 1 running for [0-9dhm]+; 1 idle for [0-9dhm]+; "
                     "1 held: Job has gone over memory limit of 4096 megabytes\\. "
                     "Peak usage: 4200 me…\\.  TAB lists its jobs\\.\\'")
             (condorit-jobs--eldoc #'ignore)))
    (condorit-test-goto 'condorit-cluster "1201")
    (should (string-match-p ": 2 completed, 1 of them failed; 1 removed\\."
                            (condorit-jobs--eldoc #'ignore)))))

(ert-deftest condorit-jobs-explain-cluster-counts ()
  (let ((now 10000))
    (should (string-match-p
             "Cluster 5 (b): 2 running, longest 1h00m; 2 held, for 2 different reasons\\."
             (condorit-jobs--explain-cluster
              5 (list '(:ClusterId 5 :JobStatus 2 :JobBatchName "b" :JobCurrentStartDate 6400)
                      '(:ClusterId 5 :JobStatus 2 :JobCurrentStartDate 9000)
                      '(:ClusterId 5 :JobStatus 5 :HoldReason "a")
                      '(:ClusterId 5 :JobStatus 5 :HoldReason "b"))
              now)))))

;;;; A selection that is not a set of sibling rows is refused

(ert-deftest condorit-jobs-mixed-region-is-refused ()
  (condorit-test-with-status-buffer ()
    (transient-mark-mode 1)
    ;; From a held job row to a cluster row in another section.
    (condorit-test-goto 'condorit-job "1234.2")
    (push-mark (point) t t)
    (condorit-test-goto 'condorit-cluster "1240")
    (should-error (condorit-jobs-at-point) :type 'user-error)
    (deactivate-mark)
    ;; Without a region the row at point is still used.
    (should (equal (condorit-jobs-at-point) '("1240")))))

(ert-deftest condorit-jobs-single-job-commands-refuse-a-selection ()
  ;; Analyze and the ClassAd view show one job; with several selected
  ;; they say so instead of quietly using the first.
  (condorit-test-with-status-buffer ()
    (transient-mark-mode 1)
    (magit-section-show (condorit-test-find-section 'condorit-cluster "1234"))
    (condorit-test-goto 'condorit-job "1234.0")
    (should (equal (condorit-job-id-at-point) "1234.0"))
    (push-mark (point) t t)
    (condorit-test-goto 'condorit-job "1234.1")
    (should (equal (condorit-jobs-at-point) '("1234.0" "1234.1")))
    (should-error (condorit-job-id-at-point) :type 'user-error)
    (deactivate-mark)))

;;;; Detail column (found by the live test against minicondor 9.0 and 25.0)

(ert-deftest condorit-jobs-hold-reason-names-the-cause ()
  ;; Real messages for a missing transfer_input_files entry (code 13).
  (dolist (reason
           '("Error from slot1@node.example.org: SHADOW at 127.0.0.1 failed to send file(s) to <127.0.0.1:30075>: error reading from /work/jobs/does-not-exist.dat: (errno 2) No such file or directory; STARTER failed to receive file(s) from <127.0.0.1:9718>"
             "Transfer input files failure at access point submit while sending files to execution point slot1_1@node.example.org. Details: 1 total failures: first failure: reading from file /work/jobs/does-not-exist.dat: (errno 2) No such file or directory"))
    (should (equal (substring-no-properties
                    (condorit-jobs--short-reason (list :HoldReason reason :HoldReasonCode 13)))
                   "input transfer: does-not-exist.dat: No such file or directory")))
  ;; Without a file and errno, only the "Error from SLOT: " prefix goes.
  (should (equal (substring-no-properties
                  (condorit-jobs--short-reason
                   '(:HoldReason "Error from slot1@n: Job has gone over memory limit." :HoldReasonCode 34)))
                 "Job has gone over memory limit."))
  (should (equal (substring-no-properties
                  (condorit-jobs--short-reason '(:HoldReason "via condor_hold (by user alice)" :HoldReasonCode 1)))
                 "via condor_hold (by user alice)")))

(ert-deftest condorit-jobs-detail-of-finished-and-idle-jobs ()
  (let ((now 10000))
    (should (equal (substring-no-properties
                    (condorit-jobs--detail '(:JobStatus 4 :ExitCode 0 :CompletionDate 2800) now))
                   "2h00m ago"))
    (should (equal (substring-no-properties
                    (condorit-jobs--detail '(:JobStatus 4 :ExitCode 2 :CompletionDate 9700) now))
                   "✗ exit 2 · 5m ago"))
    ;; Removed jobs have CompletionDate 0; they left at EnteredCurrentStatus.
    (should (equal (substring-no-properties
                    (condorit-jobs--detail '(:JobStatus 3 :CompletionDate 0 :EnteredCurrentStatus 9940
                                            :RemoveReason "via condor_rm (by user alice)")
                                          now))
                   "1m ago · via condor_rm (by user alice)"))
    (should (equal (substring-no-properties
                    (condorit-jobs--detail '(:JobStatus 1 :EnteredCurrentStatus 6400) now))
                   "1h00m"))
    (should (equal (condorit-jobs--detail '(:JobStatus 1) now) ""))))

(ert-deftest condorit-jobs-flag-idle-jobs-that-fit-no-slot ()
  (with-temp-buffer
    (use-local-map condorit-status-mode-map)
    (setq condorit-jobs--capacities '((32 128000 nil)))
    (let ((now 10000)
          (big '(:ClusterId 7 :ProcId 0 :JobStatus 1 :EnteredCurrentStatus 6400
                 :RequestMemory 921600))
          (small '(:ClusterId 7 :ProcId 1 :JobStatus 1 :EnteredCurrentStatus 6400
                   :RequestMemory 1024)))
      (should (equal (substring-no-properties (condorit-jobs--detail big now))
                     "1h00m · ✗ fits no slot: memory 900.0G > 125.0G"))
      (should (equal (substring-no-properties (condorit-jobs--detail small now)) "1h00m"))
      (should (string-match-p "no slot of the pool is big enough"
                              (condorit-jobs--explain big now)))
      (should (string-match-p "Press a" (condorit-jobs--explain small now)))
      (should (string-match-p "✗ 1 too big for any slot"
                              (condorit-jobs--cluster-heading (list big small) now "  " nil)))
      ;; A held job with the same request is not flagged.
      (should-not (condorit-jobs--misfit (plist-put (copy-sequence big) :JobStatus 5))))))

(ert-deftest condorit-jobs-memory-usage ()
  (let ((now 10000)
        (running '(:ClusterId 8 :ProcId 0 :JobStatus 2 :JobCurrentStartDate 6400
                   :RequestMemory 4096 :ResidentSetSize 3984588))
        (done '(:ClusterId 8 :ProcId 1 :JobStatus 4 :ExitCode 0 :CompletionDate 9700
                :RequestMemory 8192 :ResidentSetSize 614400)))
    ;; Close to the limit: the warning face.
    (let ((detail (condorit-jobs--detail running now)))
      (should (equal (substring-no-properties detail) "1h00m · mem 3.8G/4.0G"))
      (should (eq (get-text-property (- (length detail) 1) 'font-lock-face detail)
                  'condorit-hold-reason)))
    (let ((detail (condorit-jobs--detail done now)))
      (should (equal (substring-no-properties detail) "5m ago · mem 600M/8.0G"))
      (should (eq (get-text-property (- (length detail) 1) 'font-lock-face detail)
                  'condorit-number)))
    ;; A cluster shows the largest usage and request of its jobs.
    (should (string-match-p "^  8 .* [0-9]+h[0-9]+m · mem 3.8G/8.0G$"
                            (condorit-jobs--cluster-heading (list running done) now "  " nil)))
    (should (string-match-p "(95%): condor holds it if it goes over"
                            (condorit-jobs--explain running now)))
    (should (string-match-p "Peak memory 600M of 8.0G requested (7%): a smaller request_memory"
                            (condorit-jobs--explain done now)))
    ;; Removed jobs show no memory, nor does their cluster.
    (should-not (string-match-p "mem" (condorit-jobs--cluster-heading
                                       (list (plist-put (copy-sequence done) :JobStatus 3))
                                       now "  " t)))
    ;; A small request is not worth shrinking.
    (should (string-match-p "(5%)\\.$" (condorit-jobs--explain
                                          '(:JobStatus 4 :ExitCode 0 :RequestMemory 512
                                            :ResidentSetSize 27500)
                                          now)))
    ;; No usage reported (e.g. idle): nothing shown.
    (should-not (condorit-jobs--memory '((:JobStatus 1 :RequestMemory 1024))))))

(ert-deftest condorit-jobs-ssh-only-to-running-jobs ()
  (with-temp-buffer
    (condorit-jobs--index '((:ClusterId 5 :ProcId 0 :JobStatus 1)
                           (:ClusterId 5 :ProcId 1 :JobStatus 2)
                           (:ClusterId 6 :ProcId 0 :JobStatus 5)))
    (should-not (condorit-actions--check-running "5.1"))
    (should-not (condorit-actions--check-running "5"))
    (should-error (condorit-actions--check-running "5.0") :type 'user-error)
    (should-error (condorit-actions--check-running "6") :type 'user-error)))

(ert-deftest condorit-jobs-ssh-to-job-runs-in-a-terminal ()
  (condorit-test-with-fake-condor '(("condor_ssh_to_job" :out "hello from the job\n"))
    ;; term starts the program through /bin/sh, which searches PATH.
    (let ((name "*condorit-ssh: 1234.0*")
          (process-environment (cons (concat "PATH=" condorit-test-bin-dir ":" (getenv "PATH"))
                                     process-environment)))
      (unwind-protect
          (progn
            (save-window-excursion (condorit-ssh-to-job "1234.0"))
            (with-current-buffer name
              (should (derived-mode-p 'term-mode))
              (condorit-test-wait (lambda () (not (get-buffer-process (current-buffer)))))
              (should (string-match-p "hello from the job" (buffer-string))))
            (should (equal (condorit-test-argv) '(("condor_ssh_to_job" "1234.0"))))
            (with-current-buffer condorit-log-buffer-name
              (should (string-match-p "\\$ condor_ssh_to_job 1234.0$" (buffer-string)))))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest condorit-jobs-ssh-to-job-uses-the-terminal-function ()
  (let* (called
         (condorit-terminal-function
          (lambda (&rest args) (setq called (cons default-directory args))))
         (default-directory "/ssh:alice@submit:/home/alice/"))
    (condorit-ssh-to-job "36.0")
    ;; The terminal function runs it on the view's host.
    (should (equal called '("/ssh:alice@submit:/home/alice/" "*condorit-ssh@submit: 36.0*"
                            "condor_ssh_to_job" ("36.0"))))))

(ert-deftest condorit-jobs-term-runs-through-tramp ()
  (condorit-test-with-remote
    (let ((name "*condorit-test-term*"))
      (unwind-protect
          (progn
            (save-window-excursion
              (condorit-term name "sh" '("-c" "echo remote $(pwd); tty >/dev/null && echo has-tty")))
            (with-current-buffer name
              (condorit-test-wait (lambda () (not (get-buffer-process (current-buffer)))) 20)
              ;; A tty, and every line starts at the left: the carriage
              ;; returns arrived.
              (should (string-match-p "^has-tty$" (buffer-string)))
              (should (string-match-p (concat "^remote " (regexp-quote
                                                          (directory-file-name
                                                           (file-local-name default-directory)))
                                              "$")
                                      (buffer-string)))))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest condorit-jobs-tail-follows-output ()
  (condorit-test-with-fake-condor '(("condor_tail" :out "step 1\nstep 2\n"))
    (let ((condorit-tail-bytes 4096))
      (save-window-excursion
        (condorit-tail "1234.0")
        (condorit-tail "1234.0" t))
      (condorit-test-in-buffer "*condorit-tail: 1234.0*"
        (should (derived-mode-p 'condorit-tail-mode))
        (condorit-test-wait (lambda () (not (get-buffer-process (current-buffer)))))
        (should (string-match-p "\\`\\$ condor_tail -f -maxbytes 4096 1234.0\n\nstep 1\nstep 2\n"
                                (buffer-string)))
        (should (string-match-p "\\[condor_tail finished\\]" (buffer-string))))
      (condorit-test-in-buffer "*condorit-tail: 1234.0 stderr*"
        (condorit-test-wait (lambda () (not (get-buffer-process (current-buffer))))))
      ;; The two run at once, so they may log in either order.
      (should (seq-set-equal-p
               (condorit-test-argv)
               '(("condor_tail" "-f" "-maxbytes" "4096" "1234.0")
                 ("condor_tail" "-f" "-maxbytes" "4096" "-no-stdout" "-stderr" "1234.0")))))))

(ert-deftest condorit-jobs-tail-selected-and-quits-with-q ()
  (condorit-test-with-fake-condor '(("condor_tail" :out "step 1\n"))
    (save-window-excursion
      (condorit-tail "1234.0")
      (let ((buffer (current-buffer)))
        (should (equal (buffer-name buffer) "*condorit-tail: 1234.0*"))
        (should (eq (window-buffer (selected-window)) buffer))
        (should (eq (key-binding "q") 'condorit-tail-quit))
        (call-interactively #'condorit-tail-quit)
        (should-not (buffer-live-p buffer))))))

(ert-deftest condorit-jobs-tail-only-running-jobs ()
  (with-temp-buffer
    (condorit-jobs--index '((:ClusterId 5 :ProcId 0 :JobStatus 4)))
    (should-error (condorit-tail "5.0") :type 'user-error)))

(ert-deftest condorit-jobs-tail-remote ()
  (condorit-test-with-fake-condor '(("condor_tail" :out "remote output\n"))
    (condorit-test-with-remote
      (let ((name (condorit--buffer-name "condorit-tail" "7.0")))
        (save-window-excursion (condorit-tail "7.0"))
        (condorit-test-in-buffer name
          (condorit-test-wait (lambda () (not (get-buffer-process (current-buffer)))) 20)
          (should (string-match-p "^remote output$" (buffer-string))))))))

(defconst condorit-jobs-test--sortable
  '((:ClusterId 1 :ProcId 0 :JobStatus 2 :JobBatchName "beta" :JobCurrentStartDate 100
     :ResidentSetSize 1024000)
    (:ClusterId 1 :ProcId 1 :JobStatus 2 :JobBatchName "beta" :JobCurrentStartDate 300
     :ResidentSetSize 4096000)
    (:ClusterId 2 :ProcId 0 :JobStatus 1 :JobBatchName "alpha" :EnteredCurrentStatus 200)
    (:ClusterId 3 :ProcId 0 :JobStatus 2 :JobBatchName "gamma" :JobCurrentStartDate 50
     :ResidentSetSize 2048000)
    (:ClusterId 4 :ProcId 0 :JobStatus 3 :JobBatchName "delta" :EnteredCurrentStatus 10
     :ResidentSetSize 9999999))
  "Four clusters; cluster 2 has no memory usage, removed cluster 4 shows none.")

(defun condorit-jobs-test--order (jobs)
  "The job ids of JOBS as grouped and sorted in the current buffer."
  (mapcan (lambda (group) (mapcar #'condorit-classad-job-id (cdr group)))
          (condorit-jobs--group-clusters jobs)))

(ert-deftest condorit-jobs-sort-orders ()
  (with-temp-buffer
    (let ((jobs condorit-jobs-test--sortable))
      (should (equal (condorit-jobs-test--order jobs) '("1.0" "1.1" "2.0" "3.0" "4.0")))
      ;; Memory, largest first: cluster 1 goes where its biggest job does.
      ;; Clusters 2 and 4 show none and go last, in id order.
      (setq condorit-jobs--sort '("memory" . t))
      (should (equal (condorit-jobs-test--order jobs) '("1.1" "1.0" "3.0" "2.0" "4.0")))
      (setq condorit-jobs--sort '("memory"))
      (should (equal (condorit-jobs-test--order jobs) '("1.0" "1.1" "3.0" "2.0" "4.0")))
      (setq condorit-jobs--sort '("time" . t))
      (should (equal (condorit-jobs-test--order jobs) '("1.1" "1.0" "2.0" "3.0" "4.0")))
      (setq condorit-jobs--sort '("name"))
      (should (equal (condorit-jobs-test--order jobs) '("2.0" "1.0" "1.1" "4.0" "3.0"))))))

(ert-deftest condorit-jobs-sort-command ()
  (with-temp-buffer
    (condorit-history-mode)
    (condorit-jobs-sort "name")
    (should (equal condorit-jobs--sort '("name")))
    ;; The same key again reverses.
    (condorit-jobs-sort "name")
    (should (equal condorit-jobs--sort '("name" . t)))
    (condorit-jobs-sort "memory")
    (should (equal condorit-jobs--sort '("memory" . t)))
    (condorit-jobs-sort "id")
    (should-not condorit-jobs--sort)
    ;; id reverses too, and back to the default.
    (condorit-jobs-sort "id")
    (should (equal condorit-jobs--sort '("id" . t)))
    (should (equal (condorit-jobs-test--order condorit-jobs-test--sortable)
                   '("4.0" "3.0" "2.0" "1.1" "1.0")))
    (condorit-jobs-sort "id")
    (should-not condorit-jobs--sort)
    ;; Extra columns sort by their title.
    (let ((condorit-job-columns '(("JobPrio" "PRIO" 4 right))))
      (should (assoc "prio" (condorit-jobs--sort-keys))))))

(ert-deftest condorit-jobs-other-schedd ()
  (should (equal (condorit-jobs--normalize-ids '("a.org#12" "a.org#12.0" "b.org#12.0"))
                 '("a.org#12" "b.org#12.0")))
  (should (equal (condorit-actions--args '("-forcex") '("a.org#12.0" "a.org#13"))
                 '("-name" "a.org" "-forcex" "12.0" "13")))
  (should (equal (condorit-actions--qedit-args '("a.org#12.0") "JobPrio" "5")
                 '("-name" "a.org" "-constraint" "(ClusterId == 12 && ProcId == 0)"
                   "JobPrio" "5")))
  (should-error (condorit-actions--args nil '("a.org#12" "13")) :type 'user-error)
  (condorit-test-with-fake-condor '(("condor_release" :out "Job 12.0 released\n"))
    (let (refreshed)
      (cl-letf (((symbol-function 'condorit-refresh-all) (lambda (&rest _) (setq refreshed t))))
        (condorit-release '("a.org#12.0"))
        (condorit-test-wait (lambda () refreshed))
        (should (equal (condorit-test-argv)
                       '(("condor_release" "-name" "a.org" "12.0"))))))))

;;; condorit-jobs-test.el ends here
