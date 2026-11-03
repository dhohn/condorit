;;; condorit-status-test.el --- Tests for condorit-status data  -*- lexical-binding: t; -*-

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

(defun condorit-test-jobs ()
  (condorit-classad-parse (condorit-test-fixture "condor_q.json")))

(ert-deftest condorit-status-queue-args ()
  (let ((args (condorit-status--queue-args '("-allusers" "-constraint=JobPrio > 0"))))
    (should (equal (seq-take args 2) '("-json" "-attributes")))
    (should (string-prefix-p "ClusterId,ProcId,JobStatus," (nth 2 args)))
    (should (equal (nthcdr 3 args) '("-allusers" "-constraint" "JobPrio > 0")))))

(ert-deftest condorit-status-history-args ()
  (let ((condorit-history-limit 1000))
    (should (equal (nthcdr 3 (condorit-status--history-args nil "alice" 1791883600))
                   '("-since" "EnteredCurrentStatus <= 1791883600" "-limit" "1000" "alice")))
    (should (equal (nthcdr 3 (condorit-status--history-args '("-allusers" "-constraint=X") "alice" 5))
                   '("-since" "EnteredCurrentStatus <= 5" "-limit" "1000" "-constraint" "X")))))

(ert-deftest condorit-status-user-priority-through-groups ()
  ;; Seen in a real pool: nice_user jobs and accounting groups give the
  ;; same person more submitter names.
  (let ((ads '((:Name "nice-user.alice@example.org" :Priority 9000.0)
               (:Name "group_physics.alice@example.org" :Priority 700.0)
               (:Name "malice@example.org" :Priority 1.0))))
    (should (= (condorit-status--user-priority ads "alice") 9000.0))
    (should (= (condorit-status--user-priority
                (cons '(:Name "alice@example.org" :Priority 500.0) ads) "alice")
               500.0))
    (should (null (condorit-status--user-priority ads "bob")))))

(ert-deftest condorit-status-group-clusters ()
  (let ((groups (condorit-jobs--group-clusters (condorit-test-jobs))))
    (should (equal (mapcar #'car groups) '("1234" "1240" "1241" "1300")))
    (should (equal (mapcar (lambda (j) (plist-get j :ProcId)) (cdr (assoc "1234" groups)))
                   '(0 1 2)))))

(ert-deftest condorit-status-group-owners ()
  (should (equal (mapcar #'car (condorit-status--group-owners (condorit-test-jobs)))
                 '("alice" "bob"))))

(ert-deftest condorit-status-state-counts ()
  (let ((groups (condorit-jobs--group-clusters (condorit-test-jobs))))
    (should (equal (condorit-jobs--state-counts (cdr (assoc "1234" groups))) "R 1  I 1  H 1"))
    (should (equal (condorit-jobs--state-counts
                    (condorit-classad-parse (condorit-test-fixture "condor_history.json")))
                   "C 2  X 1"))))

(ert-deftest condorit-status-longest-running ()
  (let ((groups (condorit-jobs--group-clusters (condorit-test-jobs))))
    (should (= (condorit-jobs--longest-running (cdr (assoc "1234" groups)) condorit-test-now) 8000))
    (should (null (condorit-jobs--longest-running (cdr (assoc "1240" groups)) condorit-test-now)))
    (should (null (condorit-jobs--longest-running '((:JobStatus 2)) condorit-test-now)))))

(ert-deftest condorit-status-pool-summary ()
  (should (equal (condorit-classad-pool-summary
                  (condorit-classad-parse (condorit-test-fixture "condor_status.json")))
                 '(:machines 2 :cpus-total 96 :cpus-used 4 :jobs 1 :gpus-total 0 :gpus-used 0)))
  (should (equal (condorit-classad-pool-summary nil)
                 '(:machines 0 :cpus-total 0 :cpus-used 0 :jobs 0 :gpus-total 0 :gpus-used 0))))

(ert-deftest condorit-status-user-priority ()
  (let ((ads (condorit-classad-parse-long (condorit-test-fixture "condor_userprio.long"))))
    (should (= (condorit-status--user-priority ads "alice") 500.0))
    (should (null (condorit-status--user-priority ads "carol")))))

(ert-deftest condorit-status-fetch ()
  (condorit-test-with-fake-condor (condorit-test-fixture-responses)
    (with-temp-buffer
      (condorit-status-mode)
      (setq condorit--user "alice")
      (condorit-refresh)
      (condorit-test-wait (lambda () (not condorit--pending)))
      (should-not condorit--error)
      (should (= (length (plist-get condorit--data :finished)) 3))
      (should (equal (car (last (assoc "condor_history" (condorit-test-argv)))) "alice"))
      (should (member '("condor_userprio" "-allusers" "-long") (condorit-test-argv))))))

(ert-deftest condorit-status-parse-results-indexed-userprio ()
  (let ((r (condorit--parse-results
            condorit-test-userprio-spec
            `((userprio "condor_userprio" 0
                        ,(condorit-test-fixture "condor_userprio_allusers.long") ""))
            0)))
    (should (= (condorit-status--user-priority (plist-get r :userprio) "bob") 2250.0))))

(defun condorit-status-test--states (&rest pairs)
  "A job id -> state table from PAIRS (ID STATE ...)."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(ert-deftest condorit-status-changes ()
  (let ((old (condorit-status-test--states "10.0" 'running "10.1" 'running "10.2" 'idle
                                          "11.0" 'running "11.1" 'idle
                                          "12.0" 'idle "13.0" 'held))
        (jobs '((:ClusterId 10 :ProcId 2 :JobStatus 2 :Owner "alice")
                (:ClusterId 11 :ProcId 0 :JobStatus 5 :Owner "alice")
                (:ClusterId 11 :ProcId 1 :JobStatus 5 :Owner "alice")
                (:ClusterId 13 :ProcId 0 :JobStatus 5 :Owner "alice")))
        (finished '((:ClusterId 10 :ProcId 0 :JobStatus 4 :ExitCode 0 :Owner "alice")
                    (:ClusterId 10 :ProcId 1 :JobStatus 4 :ExitCode 3 :Owner "alice"))))
    ;; 12.0 vanished without a history entry (e.g. new options): not reported.
    ;; 13.0 was already held: not reported again.
    (should (equal (condorit-status--changes old jobs finished "alice")
                   '("cluster 10: 1 completed, 1 failed" "cluster 11: 2 held"))))
  ;; A cluster with nothing left in the queue is done.
  (should (equal (condorit-status--changes
                  (condorit-status-test--states "20.0" 'running "20.1" 'running)
                  nil
                  '((:ClusterId 20 :ProcId 0 :JobStatus 4 :ExitCode 0 :Owner "alice")
                    (:ClusterId 20 :ProcId 1 :JobStatus 3 :Owner "alice"))
                  "alice")
                 '("cluster 20 done (1 completed, 1 removed)")))
  ;; Other users' jobs never count.
  (should-not (condorit-status--changes
               (condorit-status-test--states "30.0" 'running)
               '((:ClusterId 30 :ProcId 0 :JobStatus 5 :Owner "bob"))
               nil "alice")))

(ert-deftest condorit-status-notifies-once-per-refresh ()
  (let* (notes
         (condorit-notify-function (lambda (text) (push text notes)))
         (running '(:ClusterId 50 :ProcId 0 :JobStatus 2 :Owner "alice"))
         (held '(:ClusterId 50 :ProcId 0 :JobStatus 5 :Owner "alice")))
    (with-temp-buffer
      (condorit-status-mode)
      (setq condorit--user "alice")
      ;; The first refresh only remembers the states.
      (condorit--receive (current-buffer) (list :jobs (list running) :time 0) nil)
      (should-not notes)
      (condorit--receive (current-buffer) (list :jobs (list held) :time 1) nil)
      ;; Redrawing (e.g. "Refreshing…" or sorting) reports nothing.
      (condorit--render)
      (should (equal notes '("cluster 50: 1 held"))))))

;;; condorit-status-test.el ends here
