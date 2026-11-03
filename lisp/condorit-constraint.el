;;; condorit-constraint.el --- Help writing ClassAd constraints  -*- lexical-binding: t; -*-

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

;;; Commentary:
;; Minibuffer readers for -constraint options.  M-n offers expressions
;; built from the row at point, TAB completes attribute names (with
;; their meaning and units), and RET first asks condor how many jobs or
;; slots the expression matches.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'condorit-process)
(require 'condorit-classad)
(require 'condorit-base)
(require 'condorit-jobs)

(defconst condorit-constraint-job-attributes
  '(("ClusterId" . "cluster number")
    ("ProcId" . "job number within its cluster")
    ("JobStatus" . "1 idle, 2 running, 3 removed, 4 completed, 5 held, 6 transferring, 7 suspended")
    ("Owner" . "user who submitted it")
    ("JobBatchName" . "batch name")
    ("Cmd" . "executable")
    ("QDate" . "submit time, Unix seconds: time() - QDate > 3600")
    ("EnteredCurrentStatus" . "when it entered its state, Unix seconds")
    ("JobCurrentStartDate" . "start of the current run, Unix seconds")
    ("CompletionDate" . "end time, Unix seconds; 0 unless completed")
    ("RequestCpus" . "CPUs requested")
    ("RequestMemory" . "memory requested, MiB")
    ("RequestDisk" . "disk requested, KiB")
    ("RequestGPUs" . "GPUs requested")
    ("ResidentSetSize" . "peak memory used, KiB")
    ("RemoteHost" . "slot it runs on")
    ("HoldReasonCode" . "why it is held, e.g. 34 = over its memory request")
    ("HoldReason" . "hold message: regexp(\"memory\", HoldReason)")
    ("ExitCode" . "exit code, once completed")
    ("NumJobStarts" . "how often it started")
    ("RemoteWallClockTime" . "wall-clock time used, seconds")
    ("JobPrio" . "priority among your jobs, higher first"))
  "Common job attributes with their meaning and units.")

(defconst condorit-constraint-slot-attributes
  '(("Machine" . "host name")
    ("Name" . "slot name")
    ("State" . "Unclaimed, Claimed, Owner, Matched or Drained")
    ("Activity" . "Idle, Busy, Retiring or Vacating")
    ("SlotType" . "Static, Partitionable or Dynamic")
    ("Cpus" . "CPUs of the slot; free ones for a partitionable slot")
    ("TotalCpus" . "CPUs of the machine")
    ("Memory" . "memory of the slot, MiB; free for a partitionable slot")
    ("TotalMemory" . "memory of the machine, MiB")
    ("Disk" . "disk of the slot, KiB")
    ("GPUs" . "GPUs of the slot; free ones for a partitionable slot")
    ("TotalGPUs" . "GPUs of the machine")
    ("CUDADeviceName" . "GPU model")
    ("OpSysAndVer" . "operating system, e.g. \"Rocky8\"")
    ("Arch" . "CPU architecture, e.g. \"X86_64\"")
    ("LoadAvg" . "load of the slot")
    ("TotalLoadAvg" . "load of the machine")
    ("RemoteOwner" . "user whose job runs there"))
  "Common slot attributes with their meaning and units.")

;;;; What the view knows

(defun condorit-constraint--ad-attributes (ads)
  "Return the attribute names that occur in ADS."
  (let (names)
    (dolist (ad ads)
      (cl-loop for (key _) on ad by #'cddr
               do (push (substring (symbol-name key) 1) names)))
    (delete-dups names)))

(defun condorit-constraint--attributes (kind)
  "Return (NAME . DOC) for the attributes of KIND (`jobs' or `slots').
The curated ones come first, then those in the view's data."
  (let ((curated (if (eq kind 'slots)
                     condorit-constraint-slot-attributes
                   condorit-constraint-job-attributes))
        (seen (condorit-constraint--ad-attributes
               (if (eq kind 'slots)
                   (plist-get condorit--data :slots)
                 (append (plist-get condorit--data :jobs)
                         (plist-get condorit--data :finished))))))
    (append curated
            (mapcar #'list (seq-remove (lambda (name) (assoc name curated)) seen)))))

(defun condorit-constraint--job-examples ()
  "Return constraints built from the job or cluster at point, then general ones."
  (let ((job (ignore-errors (condorit-job-at-point))))
    (append
     (when job
       (let ((cluster (plist-get job :ClusterId))
             (batch (plist-get job :JobBatchName))
             (memory (plist-get job :RequestMemory)))
         (delq nil
               (list (format "ClusterId == %d" cluster)
                     (format "ClusterId == %d && ProcId == %d" cluster (plist-get job :ProcId))
                     (and (stringp batch) (format "JobBatchName == %S" batch))
                     (and (stringp (plist-get job :Owner))
                          (format "Owner == %S" (plist-get job :Owner)))
                     (format "JobStatus == %d" (plist-get job :JobStatus))
                     (and (numberp memory) (format "RequestMemory >= %d" memory))))))
     '("JobStatus == 5"
       "JobStatus == 2 && time() - JobCurrentStartDate > 3600"
       "time() - QDate > 86400"
       "regexp(\"memory\", HoldReason)"))))

(defun condorit-constraint--slot-examples ()
  "Return constraints built from the machine or slot at point, then general ones."
  (let* ((section (and (bound-and-true-p magit-root-section) (magit-current-section)))
         (value (and section (oref section value)))
         (slots (plist-get condorit--data :slots))
         (slot (pcase (and section (oref section type))
                 ('condorit-slot (seq-find (lambda (s) (equal (plist-get s :Name) value)) slots))
                 ('condorit-machine (seq-find (lambda (s) (equal (plist-get s :Machine) value))
                                             slots)))))
    (append
     (when slot
       (delq nil
             (list (format "Machine == %S" (plist-get slot :Machine))
                   (and (eq (oref section type) 'condorit-slot)
                        (format "Name == %S" (plist-get slot :Name)))
                   (and (stringp (plist-get slot :CUDADeviceName))
                        (format "CUDADeviceName == %S" (plist-get slot :CUDADeviceName)))
                   (and (stringp (plist-get slot :OpSysAndVer))
                        (format "OpSysAndVer == %S" (plist-get slot :OpSysAndVer))))))
     '("TotalGPUs > 0"
       "State == \"Unclaimed\""
       "Cpus >= 8"
       "Memory >= 64000"))))

;;;; Completion

(defvar-local condorit-constraint--candidates nil
  "Attributes (NAME . DOC) completed in this minibuffer.")

(defun condorit-constraint--completion-at-point ()
  "Complete the attribute name at point in a constraint minibuffer."
  (when condorit-constraint--candidates
    (let ((end (point))
          (start (save-excursion (skip-chars-backward "A-Za-z0-9_") (point))))
      ;; Also with nothing typed yet: then all attributes are offered.
      ;; Not inside a string: an even number of quotes before.
      (when (cl-evenp (cl-count ?\" (buffer-substring (minibuffer-prompt-end) start)))
        (let ((candidates condorit-constraint--candidates))
          (list start end (mapcar #'car candidates)
                :annotation-function
                (lambda (name)
                  (when-let* ((doc (cdr (assoc name candidates))))
                    (concat "  " doc)))))))))

(defvar-keymap condorit-constraint-minibuffer-map
  :doc "Keys for writing a constraint: TAB completes attribute names."
  :parent minibuffer-local-map
  "TAB" #'completion-at-point)

;;;; Checking

(defun condorit-constraint--unknown (expression known)
  "Return the identifiers in EXPRESSION that are not KNOWN attributes.
Strings, numbers, function calls and ClassAd keywords are left out."
  (let ((text (replace-regexp-in-string "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"" "\"\"" expression))
        (start 0)
        unknown)
    (while (string-match "\\_<\\([A-Za-z_][A-Za-z0-9_.]*\\)\\_>\\(\\s-*(\\)?" text start)
      (let ((call (match-beginning 2))
            (scoped (match-string 1 text)))
        (setq start (match-end 0))
        ;; MY.Memory and TARGET.Memory name Memory.
        (let ((name (car (last (split-string scoped "\\.")))))
          (unless (or call
                      (member-ignore-case name '("true" "false" "undefined" "error"))
                      (seq-some (lambda (attribute) (string-equal-ignore-case attribute name))
                                known))
            (push name unknown)))))
    (nreverse (delete-dups unknown))))

(defun condorit-constraint--check (check expression)
  "Ask condor how many ads match EXPRESSION.
CHECK is (PROGRAM ARGS WHAT): PROGRAM -constraint EXPRESSION ARGS
prints one line per matching WHAT.  Return (COUNT . MESSAGE); COUNT
is nil when condor rejects EXPRESSION."
  (pcase-let* ((`(,program ,args ,what) check)
               (`(,exit ,stdout ,stderr)
                (apply #'condorit-run-sync program
                       (append (list "-constraint" expression) args))))
    (if (not (eql exit 0))
        (cons nil (condorit-error-summary program exit stdout stderr))
      (let ((n (length (split-string stdout "\n" t))))
        (cons n (format "matches %d %s" n what))))))

;;;; Readers

(defun condorit-constraint-read (prompt initial-input history kind check)
  "Read a constraint for objects of KIND (`jobs' or `slots') with PROMPT.
INITIAL-INPUT and HISTORY are as for `read-from-minibuffer'.
\\<condorit-constraint-minibuffer-map>\\[next-history-element] offers \
expressions from the row at point, \\[completion-at-point] completes
attribute names.  CHECK is passed to `condorit-constraint--check'; an
expression condor rejects is offered again for fixing, and RET on it
unchanged uses it anyway.  Empty input means no constraint."
  (let ((candidates (condorit-constraint--attributes kind))
        (examples (if (eq kind 'slots)
                      (condorit-constraint--slot-examples)
                    (condorit-constraint--job-examples)))
        (input initial-input)
        (note "")
        rejected
        result)
    (while (not result)
      (let ((expression
             (string-trim
              (minibuffer-with-setup-hook
                  (lambda ()
                    (setq condorit-constraint--candidates candidates)
                    (add-hook 'completion-at-point-functions
                              #'condorit-constraint--completion-at-point nil t))
                (read-from-minibuffer (concat note prompt) input
                                      condorit-constraint-minibuffer-map nil history
                                      examples)))))
        (if (or (string-empty-p expression) (equal expression rejected))
            (setq result expression)
          (pcase-let ((`(,count . ,message) (condorit-constraint--check check expression)))
            (if count
                (let ((unknown (and (eql count 0)
                                    (condorit-constraint--unknown
                                     expression (mapcar #'car candidates)))))
                  (message "%s%s" message
                           (if unknown
                               (format " (unknown attribute(s): %s)"
                                       (string-join unknown ", "))
                             ""))
                  (setq result expression))
              (setq input expression
                    rejected expression
                    note (format "%s; RET again to use it anyway.  " message)))))))
    (and (not (string-empty-p result)) result)))

(defun condorit-constraint-read-queue (prompt initial-input history)
  "Read a condor_q constraint, checked against the whole queue.
PROMPT, INITIAL-INPUT and HISTORY are as for `read-from-minibuffer'."
  (condorit-constraint-read prompt initial-input history 'jobs
                           '("condor_q" ("-allusers" "-af" "ClusterId") "jobs in the queue (all users)")))

(defun condorit-constraint-read-history (prompt initial-input history)
  "Read a condor_history constraint, checked against the 100 newest matches.
PROMPT, INITIAL-INPUT and HISTORY are as for `read-from-minibuffer'."
  (condorit-constraint-read prompt initial-input history 'jobs
                           '("condor_history" ("-limit" "100" "-af" "ClusterId")
                             "finished jobs (at most 100 counted)")))

(defun condorit-constraint-read-slots (prompt initial-input history)
  "Read a condor_status constraint, checked against the pool's slots.
PROMPT, INITIAL-INPUT and HISTORY are as for `read-from-minibuffer'."
  (condorit-constraint-read prompt initial-input history 'slots
                           '("condor_status" ("-af" "Name") "slots")))

(provide 'condorit-constraint)
;;; condorit-constraint.el ends here
