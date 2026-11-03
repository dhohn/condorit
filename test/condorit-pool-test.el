;;; condorit-pool-test.el --- Tests for the pool view  -*- lexical-binding: t; -*-

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

(defun condorit-test-slots ()
  (condorit-classad-parse (condorit-test-fixture "condor_status.json")))

(ert-deftest condorit-pool-group-machines ()
  (let ((groups (condorit-classad-group-machines (condorit-test-slots))))
    (should (equal (mapcar #'car groups) '("wn01.example.org" "wn02.example.org")))
    (should (= (length (cdr (assoc "wn01.example.org" groups))) 2))))

(ert-deftest condorit-pool-machine-summary ()
  (let ((summary (condorit-classad-machine-summary
                  (cdr (assoc "wn01.example.org" (condorit-classad-group-machines (condorit-test-slots)))))))
    (should (= (plist-get summary :free) 28))
    (should (= (plist-get summary :total) 32))
    (should (= (plist-get summary :jobs) 1))))

(ert-deftest condorit-pool-render ()
  (with-temp-buffer
    (condorit-pool-mode)
    (setq condorit--data (list :slots (condorit-test-slots) :time condorit-test-now))
    (condorit--render)
    (let ((text (buffer-string)))
      (should (string-match-p "4/96 CPUs busy · 2 machine(s) · 1 slot(s) claimed" text))
      (should (string-match-p "^Machines (2)\n  MACHINE +FREE CPUS +FREE GPUS +JOBS +LOAD\n" text))
      (should (string-match-p "wn01.example.org +28/32 +1 +4.4" text))
      ;; No GPUs in this pool: no GPU count in the header, an empty column.
      (should-not (string-match-p "GPUs busy" text))
      (should (string-match-p "^    Hardware +32 CPUs · 125.0G$" text))
      (should (string-match-p "slot1_1@wn01.example.org +Claimed/Busy +4 CPUs +27.3G +alice@example.org" text))
      (should-not (string-match-p "^Users" text))
      (goto-char (point-min))
      (re-search-forward "slot1_1@wn01.example.org +\\(Claimed\\)")
      (should (eq (get-text-property (match-beginning 1) 'font-lock-face) 'condorit-slot-claimed))
      )))

(defun condorit-pool-test--hidden ()
  "Whether each machine section of the current pool buffer is folded on screen."
  (mapcar (lambda (machine) (and (condorit-test-folded-p machine) t))
          (oref (magit-get-section '((condorit-machines) (condorit-root))) children)))

(ert-deftest condorit-pool-opens-with-machines-folded ()
  (condorit-test-with-fake-condor
      `(("condor_status" :out ,(condorit-test-fixture "condor_status.json")))
    (let ((buffer (progn (condorit-pool) (current-buffer))))
      (unwind-protect
          (with-current-buffer buffer
            (condorit-test-wait (lambda () (not condorit--pending)))
            (should (equal (condorit-pool-test--hidden) '(t t)))
            ;; Unfold a machine and put point on one of its slots: a refresh
            ;; keeps that, opening the view again folds everything.
            (let ((machine (car (oref (magit-get-section '((condorit-machines) (condorit-root)))
                                      children))))
              (magit-section-show machine)
              (goto-char (oref (car (oref machine children)) start)))
            (condorit-refresh)
            (condorit-test-wait (lambda () (not condorit--pending)))
            (should (equal (condorit-pool-test--hidden) '(nil t)))
            (should (eq (oref (magit-current-section) type) 'condorit-slot))
            (condorit-pool)
            (condorit-test-wait (lambda () (not condorit--pending)))
            (should (equal (condorit-pool-test--hidden) '(t t)))
            (should (eq (oref (magit-current-section) type) 'condorit-machine)))
        (kill-buffer buffer)))))

(ert-deftest condorit-pool-refresh-end-to-end ()
  (condorit-test-with-fake-condor
      `(("condor_status" :out ,(condorit-test-fixture "condor_status.json"))
        ("condor_userprio" :out ,(condorit-test-fixture "condor_userprio.long")))
    (with-temp-buffer
      (condorit-pool-mode)
      (condorit-refresh)
      (condorit-test-wait (lambda () (not condorit--pending)))
      (should (string-match-p "^Machines (2)" (buffer-string)))
      ;; The machines view only needs condor_status.
      (should (equal (mapcar #'car (condorit-test-argv)) '("condor_status"))))))

(defconst condorit-pool-test--gpu-machine
  '((:Name "slot1@gpu1" :Machine "gpu1" :SlotType "Partitionable" :State "Unclaimed"
     :Cpus 16 :TotalSlotCpus 64 :Memory 100000 :TotalSlotMemory 512000
     :Disk 1000 :TotalSlotDisk 2097152 :GPUs 2 :TotalSlotGPUs 8
     :CUDADeviceName "NVIDIA H200" :CUDAGlobalMemoryMb 143158
     :OpSysLongName "Rocky Linux release 8.10" :Arch "X86_64" :has_avx512f t
     :CpuFamily 6 :CpuModelNumber 143
     :Draining t :DrainReason "kernel update" :Storage "fast" :Healthy nil)
    (:Name "slot1_1@gpu1" :Machine "gpu1" :SlotType "Dynamic" :State "Claimed"
     :Activity "Busy" :Cpus 48 :Memory 412000 :GPUs 6 :RemoteOwner "alice@example.org"
     :JobStart 6400))
  "One partitionable GPU machine with one job on it.")

(ert-deftest condorit-pool-machine-summary-gpus-and-draining ()
  (let ((summary (condorit-classad-machine-summary condorit-pool-test--gpu-machine)))
    (should (equal (list (plist-get summary :free) (plist-get summary :total)
                         (plist-get summary :gpus-free) (plist-get summary :gpus-total)
                         (plist-get summary :memory) (plist-get summary :disk)
                         (plist-get summary :draining))
                   '(16 64 2 8 512000 2097152 "kernel update")))))

(ert-deftest condorit-pool-machine-details ()
  (let* ((condorit-pool-site-attributes '("Storage" "Healthy" "Missing"))
         (slots condorit-pool-test--gpu-machine)
         (details (condorit-pool--details slots (condorit-classad-machine-summary slots))))
    (should (equal (mapcar (lambda (line) (cons (car line) (substring-no-properties (cdr line))))
                           details)
                   '(("Hardware" . "64 CPUs · 500.0G · 2.0G disk · 8× NVIDIA H200 (139.8G)")
                     ("System" . "Rocky Linux release 8.10 · X86_64 · Intel Sapphire Rapids · AVX-512")
                     ("Site" . "Storage fast · Healthy false")
                     ("Status" . "draining: kernel update"))))))

(ert-deftest condorit-pool-render-gpus-draining-and-runtime ()
  (with-temp-buffer
    (condorit-pool-mode)
    (setq condorit--data (list :slots condorit-pool-test--gpu-machine :time 10000))
    (condorit--render)
    (let ((text (buffer-string)))
      (should (string-match-p "CPUs busy · 6/8 GPUs busy · 1 machine(s)" text))
      (should (string-match-p "^  gpu1 +16/64 +2/8 +1 +0.0 draining$" text))
      (should (string-match-p "slot1_1@gpu1 +Claimed/Busy +48 CPUs +402.3G +6 GPUs +alice@example.org · 1h00m$"
                              text)))))

(ert-deftest condorit-pool-visit-shows-the-slot-ad ()
  (condorit-test-with-fake-condor
      `(("condor_status" :out ,(condorit-test-fixture "condor_status.json")))
    (with-temp-buffer
      (condorit-pool-mode)
      (setq condorit--data (list :slots (condorit-test-slots) :time condorit-test-now))
      (condorit--render)
      (magit-section-show-level-4-all)
      ;; On a slot: that slot.  On a machine: its partitionable slot.
      (condorit-test-goto 'condorit-slot "slot1_1@wn01.example.org")
      (call-interactively #'condorit-pool-visit)
      (condorit-test-goto 'condorit-machine "wn02.example.org")
      (call-interactively #'condorit-pool-visit)
      (should (equal (condorit-test-argv)
                     '(("condor_status" "-json" "slot1_1@wn01.example.org")
                       ("condor_status" "-json" "slot1@wn02.example.org"))))
      (condorit-test-in-buffer "*condorit-classad: slot1_1@wn01.example.org*"
        (should (string-match-p "^Machine = \"wn01.example.org\"$" (buffer-string))))
      (kill-buffer "*condorit-classad: slot1@wn02.example.org*")
      (goto-char (point-min))
      (should-error (call-interactively #'condorit-pool-visit) :type 'user-error))))

(ert-deftest condorit-pool-condor-args-leave-out-local-options ()
  (should (equal (condorit-pool--condor-args '("--free-cpus" "-constraint=TotalGPUs > 0"
                                              "--group=gpu" "--mine"))
                 '("-constraint" "TotalGPUs > 0")))
  (with-temp-buffer
    (setq condorit-options '("--mine" "-constraint=OpSys == \"LINUX\""))
    (should (equal (last (nth 2 (car (condorit-pool--commands))) 2)
                   '("-constraint" "OpSys == \"LINUX\"")))))

(defun condorit-pool-test--machines (slots)
  (mapcar (pcase-lambda (`(,machine . ,mslots))
            (list machine mslots (condorit-classad-machine-summary mslots)))
          (condorit-classad-group-machines slots)))

(ert-deftest condorit-pool-filter ()
  (let* ((machines (condorit-pool-test--machines
                    (append (condorit-test-slots) condorit-pool-test--gpu-machine)))
         (names (lambda (options user)
                  (mapcar #'car (condorit-pool--filter machines options user)))))
    (should (equal (funcall names nil "bob") '("gpu1" "wn01.example.org" "wn02.example.org")))
    (should (equal (funcall names '("--free-gpus") "bob") '("gpu1")))
    (should (equal (funcall names '("--mine") "alice") '("gpu1" "wn01.example.org")))
    (should (equal (funcall names '("--mine" "--free-gpus") "alice") '("gpu1")))
    ;; "ali" is not "alice": the user name must end at the @.
    (should (null (funcall names '("--mine") "ali")))))

(ert-deftest condorit-pool-render-filtered-and-grouped ()
  (with-temp-buffer
    (condorit-pool-mode)
    (setq condorit-options '("--free-cpus" "--group=gpu")
          condorit--data (list :slots (append (condorit-test-slots) condorit-pool-test--gpu-machine)
                              :time 10000))
    (condorit--render)
    (let ((text (buffer-string)))
      (should (string-match-p "^Options: +--free-cpus --group=gpu$" text))
      (should (string-match-p "^Machines (3)$" text))
      (should (string-match-p "^  NVIDIA H200 (1)\n    gpu1 " text))
      (should (string-match-p "^  No GPU (2)\n    wn01.example.org " text)))
    ;; Groups are open, the machines in them folded.
    (should-not (condorit-test-folded-p (condorit-test-find-section 'condorit-machine-group "No GPU")))
    (should (condorit-test-folded-p (condorit-test-find-section 'condorit-machine "gpu1")))
    (setq condorit-options '("--free-gpus"))
    (condorit--render)
    (should (string-match-p "^Machines (1 of 3)$" (buffer-string)))))

(ert-deftest condorit-pool-cpu-name ()
  (should (equal (condorit-pool--cpu-name '(:CpuFamily 25 :CpuModelNumber 1)) "AMD Milan (Zen 3)"))
  (should (equal (condorit-pool--cpu-name '(:CpuFamily 6 :CpuModelNumber 999)) "Intel model 999"))
  (should (equal (condorit-pool--cpu-name '(:CpuFamily 23 :CpuModelNumber 7)) "AMD family 23 model 7"))
  (should-not (condorit-pool--cpu-name '(:Machine "old")))
  ;; Grouping by CPU puts machines without CPUID numbers under "?".
  (should (equal (condorit-pool--group-key condorit-pool-test--gpu-machine "cpu") "Intel Sapphire Rapids"))
  (should (equal (condorit-pool--group-key (condorit-test-slots) "cpu") "?")))

(ert-deftest condorit-pool-machine-load ()
  ;; Without TotalLoadAvg: the slots' loads, leaving out -1 (not measured).
  (should (= (plist-get (condorit-classad-machine-summary
                         '((:SlotType "Partitionable" :LoadAvg 0.5)
                           (:SlotType "Dynamic" :LoadAvg -1.0)
                           (:SlotType "Dynamic" :LoadAvg 2.0)))
                        :load)
             2.5))
  ;; With it: the machine-wide load condor reports.
  (should (= (plist-get (condorit-classad-machine-summary
                         '((:SlotType "Partitionable" :LoadAvg 0.0 :TotalLoadAvg 14.44)
                           (:SlotType "Dynamic" :LoadAvg -1.0 :TotalLoadAvg 0.7)))
                        :load)
             14.44)))

;;; condorit-pool-test.el ends here
