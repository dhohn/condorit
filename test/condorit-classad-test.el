;;; condorit-classad-test.el --- Tests for condorit-classad  -*- lexical-binding: t; -*-

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
(require 'condorit-classad)
(require 'condorit-faces)

(ert-deftest condorit-classad-parse-json ()
  (let ((ads (condorit-classad-parse (condorit-test-fixture "condor_q.json"))))
    (should (= (length ads) 6))
    (should (equal (plist-get (car ads) :ClusterId) 1240))
    (should (equal (plist-get (car ads) :Cmd) "/home/alice/bin/plots.py"))))

(ert-deftest condorit-classad-parse-empty ()
  (should (null (condorit-classad-parse "")))
  (should (null (condorit-classad-parse "\n  \n"))))

(ert-deftest condorit-classad-parse-false-and-nested ()
  (let ((ad (car (condorit-classad-parse
                  "[{\"A\": false, \"ToE\": {\"How\": \"x\"}, \"L\": []}]"))))
    (should (null (plist-get ad :A)))
    (should (equal (plist-get (plist-get ad :ToE) :How) "x"))))

(ert-deftest condorit-classad-parse-garbage-signals ()
  (should-error (condorit-classad-parse "-- Failed to fetch ads") :type 'json-error))

(ert-deftest condorit-classad-parse-long ()
  (let ((ads (condorit-classad-parse-long (condorit-test-fixture "condor_userprio.long"))))
    (should (= (length ads) 3))
    (should (eq (plist-get (nth 0 ads) :IsAccountingGroup) t))
    (should (equal (plist-get (nth 1 ads) :Name) "alice@example.org"))
    (should (= (plist-get (nth 1 ads) :Priority) 500.0))
    (should (= (plist-get (nth 2 ads) :ResourcesUsed) 12))))

(ert-deftest condorit-classad-long-values ()
  (should (equal (condorit-classad--long-value "\"a \\\"q\\\" b\"") "a \"q\" b"))
  (should (equal (condorit-classad--long-value "42") 42))
  (should (equal (condorit-classad--long-value "-1.5e3") -1500.0))
  (should (eq (condorit-classad--long-value "true") t))
  (should (null (condorit-classad--long-value "false")))
  (should (equal (condorit-classad--long-value "time() - QDate") "time() - QDate")))

(ert-deftest condorit-classad-states ()
  (should (eq (condorit-classad-state '(:JobStatus 5)) 'held))
  (should (null (condorit-classad-state '(:JobStatus 99))))
  (should (equal (condorit-classad-state-name 'held) "Held"))
  (should (equal (condorit-classad-state-letter 'running) "R"))
  (should (equal (condorit-classad-state-name nil) "Unknown"))
  (should (equal (condorit-classad-state-letter nil) "?"))
  (should (eq (condorit-state-face 'held) 'condorit-state-held))
  (should (eq (condorit-state-face nil) 'default)))

(ert-deftest condorit-classad-names ()
  (should (equal (condorit-classad-job-id '(:ClusterId 12 :ProcId 3)) "12.3"))
  (should (equal (condorit-classad-batch-name '(:JobBatchName "b" :Cmd "/x/y.sh")) "b"))
  (should (equal (condorit-classad-batch-name '(:Cmd "/x/y.sh")) "y.sh"))
  (should (equal (condorit-classad-batch-name '()) "?")))

(ert-deftest condorit-classad-durations ()
  (should (equal (condorit-classad-format-duration 45) "45s"))
  (should (equal (condorit-classad-format-duration 720) "12m"))
  (should (equal (condorit-classad-format-duration 8040) "2h14m"))
  (should (equal (condorit-classad-format-duration 273600) "3d4h"))
  (should (equal (condorit-classad-format-duration -5) "0s"))
  (should (equal (condorit-classad-format-duration nil) "")))

(ert-deftest condorit-classad-memory ()
  (should (equal (condorit-classad-format-memory 512) "512M"))
  (should (equal (condorit-classad-format-memory 4096) "4.0G"))
  (should (equal (condorit-classad-format-memory nil) "")))

(ert-deftest condorit-classad-format-values ()
  (should (equal (condorit-classad-format-value "/Expr(time() - QDate)/") "time() - QDate"))
  (should (equal (condorit-classad-format-value "abc") "\"abc\""))
  (should (equal (condorit-classad-format-value 3) "3"))
  (should (equal (condorit-classad-format-value t) "true"))
  (should (equal (condorit-classad-format-value nil) "false")))

(ert-deftest condorit-classad-format-ad-sorted ()
  (should (equal (condorit-classad-format-ad '(:b 1 :A "x" :c t))
                 "A = \"x\"\nb = 1\nc = true\n")))

(ert-deftest condorit-classad-failed ()
  (should (condorit-classad-failed-p '(:JobStatus 4 :ExitCode 2)))
  (should (condorit-classad-failed-p '(:JobStatus 4 :ExitBySignal t)))
  (should-not (condorit-classad-failed-p '(:JobStatus 4 :ExitCode 0)))
  (should-not (condorit-classad-failed-p '(:JobStatus 3))))

(ert-deftest condorit-classad-parse-userprio-plain ()
  (let ((ads (condorit-classad-parse-userprio (condorit-test-fixture "condor_userprio.long"))))
    (should (= (length ads) 3))
    (should (equal (plist-get (nth 2 ads) :Name) "bob@example.org"))))

(ert-deftest condorit-classad-parse-userprio-indexed ()
  ;; condor_userprio -allusers -long prints one ad with numbered attributes.
  (let ((ads (condorit-classad-parse-userprio
              (condorit-test-fixture "condor_userprio_allusers.long"))))
    (should (= (length ads) 2))
    (should (equal (plist-get (nth 0 ads) :Name) "alice@example.org"))
    (should (= (plist-get (nth 0 ads) :Priority) 500.0))
    (should (= (plist-get (nth 1 ads) :ResourcesUsed) 12))
    (should (eq (plist-get (nth 1 ads) :IsAccountingGroup) nil))
    (should-not (plist-member (nth 0 ads) :NumSubmittors))))

(ert-deftest condorit-classad-slot-capacities ()
  ;; A partitionable slot offers its whole machine; a static one itself.
  (should (equal (condorit-classad-slot-capacities
                  '((:SlotType "Partitionable" :Cpus 4 :TotalSlotCpus 32 :Memory 1000
                     :TotalSlotMemory 128000 :Disk 10 :TotalSlotDisk 900.0)
                    (:SlotType "Static" :Cpus 1 :Memory 2048)
                    (:SlotType "Static" :Cpus 1 :Memory 2048)))
                 '((32 128000 900.0) (1 2048 nil)))))

(ert-deftest condorit-classad-misfit ()
  (let ((capacities '((32 128000 1048576) (64 256000 2097152))))
    (should-not (condorit-classad-misfit '(:RequestCpus 64 :RequestMemory 4096) capacities))
    ;; Expressions and missing requests are taken to fit.
    (should-not (condorit-classad-misfit '(:RequestDisk "/Expr(1024 * 1024)/") capacities))
    (should (equal (condorit-classad-misfit '(:RequestMemory 921600) capacities)
                   "memory 900.0G > 250.0G"))
    (should (equal (condorit-classad-misfit '(:RequestCpus 128 :RequestDisk 4194304) capacities)
                   "CPUs 128 > 64, disk 4.0G > 2.0G"))
    ;; Each request fits some slot, but no slot fits them all.
    (should (equal (condorit-classad-misfit '(:RequestCpus 64 :RequestMemory 200000)
                                           '((64 1000 nil) (1 256000 nil)))
                   "no single slot has all the requested CPUs, memory and disk"))
    ;; Unknown pool or unknown capacity: never flag.
    (should-not (condorit-classad-misfit '(:RequestMemory 921600) nil))
    (should-not (condorit-classad-misfit '(:RequestMemory 921600) '((1 nil nil))))))

(ert-deftest condorit-classad-memory-used ()
  ;; Condor's MemoryUsage: ResidentSetSize in KiB, rounded up to MiB.
  (should (= (condorit-classad-memory-used '(:ResidentSetSize 27500)) 27))
  (should (= (condorit-classad-memory-used '(:ResidentSetSize 1024)) 1))
  (should-not (condorit-classad-memory-used '(:ResidentSetSize 0)))
  (should-not (condorit-classad-memory-used '(:JobStatus 1))))

(ert-deftest condorit-classad-schedd-ids ()
  ;; Under condor_q -global, ids name the schedd, as GlobalJobId does.
  (let ((jobs (condorit-classad-mark-schedds
               '((:ClusterId 12 :ProcId 3 :GlobalJobId "a.example.org#12.3#1791970000")
                 (:ClusterId 5 :ProcId 0)))))
    (should (equal (mapcar #'condorit-classad-job-id jobs) '("a.example.org#12.3" "5.0")))
    (should (equal (mapcar #'condorit-classad-cluster-id jobs) '("a.example.org#12" "5"))))
  (should (equal (condorit-classad-split-id "a.example.org#12.3") '("a.example.org" . "12.3")))
  (should (equal (condorit-classad-split-id "12") '(nil . "12"))))

;;; condorit-classad-test.el ends here
