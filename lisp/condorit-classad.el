;;; condorit-classad.el --- Parse and describe HTCondor ClassAds  -*- lexical-binding: t; -*-

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
;; Pure functions turning condor's -json and -long output into plists,
;; describing job attributes for humans, and summing up slots.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst condorit-classad-job-attributes
  '("ClusterId" "ProcId" "JobStatus" "Owner" "JobBatchName" "Cmd" "QDate"
    "JobCurrentStartDate" "EnteredCurrentStatus" "CompletionDate"
    "HoldReason" "HoldReasonCode" "HoldReasonSubCode" "RemoveReason"
    "RequestMemory" "RequestCpus" "RequestDisk" "JobPrio" "RemoteHost"
    "ExitCode" "ExitBySignal" "Iwd" "UserLog" "Out" "Err" "NumJobStarts"
    "ResidentSetSize" "GlobalJobId")
  "Job attributes condorit asks condor_q and condor_history for.")

(defconst condorit-classad-slot-attributes
  '("Name" "Machine" "State" "Activity" "SlotType" "Cpus" "Memory" "Disk"
    "TotalSlotCpus" "TotalSlotMemory" "TotalSlotDisk" "LoadAvg" "RemoteOwner")
  "Slot attributes condorit asks condor_status for.")

(defun condorit-classad-parse (string)
  "Parse condor -json output STRING into a list of plists.
Keys are keywords named after the attributes, e.g. `:ClusterId'.
Blank output (condor prints nothing when there are no results) is nil.
Signals `json-error' when STRING is not JSON."
  (if (string-blank-p string)
      nil
    (json-parse-string string :object-type 'plist :array-type 'list
                       :null-object nil :false-object nil)))

(defun condorit-classad--long-value (string)
  "Convert the right-hand side STRING of a -long line to a Lisp value.
Strings, numbers and booleans are converted; anything else (an
expression) is returned as its source text."
  (cond ((string-prefix-p "\"" string)
         (condition-case nil (car (read-from-string string)) (error string)))
        ((string-match-p "\\`-?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?\\'" string)
         (string-to-number string))
        ((equal string "true") t)
        ((member string '("false" "undefined")) nil)
        (t string)))

(defun condorit-classad-parse-long (string)
  "Parse condor -long output STRING (blank-line separated ads) into plists."
  (let (ads current)
    (dolist (line (split-string string "\n"))
      (cond ((string-blank-p line)
             (when current (push (nreverse current) ads))
             (setq current nil))
            ((string-match "\\`\\([A-Za-z_][A-Za-z0-9_.]*\\) = \\(.*\\)\\'" line)
             (let ((key (intern (concat ":" (match-string 1 line))))
                   (raw (match-string 2 line)))
               (push key current)
               (push (condorit-classad--long-value raw) current)))))
    (when current (push (nreverse current) ads))
    (nreverse ads)))

(defconst condorit-classad-job-states
  '((1 idle "Idle" "I") (2 running "Running" "R") (3 removed "Removed" "X")
    (4 completed "Completed" "C") (5 held "Held" "H")
    (6 transferring "Transferring output" ">") (7 suspended "Suspended" "S"))
  "HTCondor JobStatus codes as (CODE STATE NAME LETTER).")

(defun condorit-classad-state (ad)
  "Return the state of job AD as a symbol, e.g. `held', or nil if unknown."
  (nth 1 (assq (plist-get ad :JobStatus) condorit-classad-job-states)))

(defun condorit-classad--state-entry (state)
  "Return the entry of STATE in `condorit-classad-job-states', or nil."
  (seq-find (lambda (entry) (eq (nth 1 entry) state)) condorit-classad-job-states))

(defun condorit-classad-state-name (state)
  "Return the name of STATE, e.g. \"Held\" for `held'."
  (or (nth 2 (condorit-classad--state-entry state)) "Unknown"))

(defun condorit-classad-state-letter (state)
  "Return the one-letter abbreviation of STATE, as condor_q uses."
  (or (nth 3 (condorit-classad--state-entry state)) "?"))

(defun condorit-classad--schedd-prefix (ad)
  "Return \"SCHEDD#\" if AD was marked with its schedd, else \"\"."
  (if-let* ((schedd (plist-get ad :condorit-schedd)))
      (concat schedd "#")
    ""))

(defun condorit-classad-job-id (ad)
  "Return the job id \"CLUSTER.PROC\" of AD.
For a job marked with its schedd (see `condorit-classad-mark-schedds') it is
\"SCHEDD#CLUSTER.PROC\", as in its GlobalJobId."
  (format "%s%s.%s" (condorit-classad--schedd-prefix ad)
          (plist-get ad :ClusterId) (plist-get ad :ProcId)))

(defun condorit-classad-cluster-id (ad)
  "Return the id of AD's cluster, \"CLUSTER\" or \"SCHEDD#CLUSTER\"."
  (format "%s%s" (condorit-classad--schedd-prefix ad) (plist-get ad :ClusterId)))

(defun condorit-classad-mark-schedds (ads)
  "Mark each of the job ADS with the schedd named in its GlobalJobId.
Their ids then include it: under condor_q -global, cluster numbers of
different schedds collide."
  (mapcar (lambda (ad)
            (let ((global (plist-get ad :GlobalJobId)))
              (if (and (stringp global) (string-match "\\`\\([^#]+\\)#" global))
                  (append ad (list :condorit-schedd (match-string 1 global)))
                ad)))
          ads))

(defun condorit-classad-split-id (id)
  "Split job or cluster ID into (SCHEDD . LOCAL-ID); SCHEDD is nil if not named."
  (if (string-match "\\`\\([^#]+\\)#\\(.*\\)\\'" id)
      (cons (match-string 1 id) (match-string 2 id))
    (cons nil id)))

(defun condorit-classad-batch-name (ad)
  "Return a human name for AD: its JobBatchName, else its executable's name."
  (or (plist-get ad :JobBatchName)
      (let ((cmd (plist-get ad :Cmd)))
        (and (stringp cmd) (file-name-nondirectory cmd)))
      "?"))

(defun condorit-classad-format-duration (seconds)
  "Format SECONDS compactly: 45s, 12m, 2h14m, 3d4h.  Non-numbers give \"\"."
  (if (not (numberp seconds))
      ""
    (let ((s (max 0 (floor seconds))))
      (cond ((< s 60) (format "%ds" s))
            ((< s 3600) (format "%dm" (/ s 60)))
            ((< s 86400) (format "%dh%02dm" (/ s 3600) (% (/ s 60) 60)))
            (t (format "%dd%dh" (/ s 86400) (% (/ s 3600) 24)))))))

(defun condorit-classad-format-memory (mib)
  "Format MIB megabytes as 512M or 4.0G.  Non-numbers give \"\"."
  (cond ((not (numberp mib)) "")
        ((< mib 1024) (format "%dM" mib))
        (t (format "%.1fG" (/ mib 1024.0)))))

(defun condorit-classad-format-value (value)
  "Format attribute VALUE the way ClassAds print it."
  (cond ((null value) "false")
        ((eq value t) "true")
        ((and (stringp value) (string-match "\\`/Expr(\\(.*\\))/\\'" value))
         (match-string 1 value))
        ((stringp value) (prin1-to-string value))
        ((numberp value) (number-to-string value))
        (t (prin1-to-string value))))

(defun condorit-classad-format-ad (ad)
  "Format AD as \"Attr = value\" lines sorted case-insensitively."
  (let (pairs)
    (while ad
      (push (cons (substring (symbol-name (car ad)) 1) (cadr ad)) pairs)
      (setq ad (cddr ad)))
    (mapconcat (lambda (pair)
                 (format "%s = %s\n" (car pair) (condorit-classad-format-value (cdr pair))))
               (sort pairs (lambda (a b) (string< (downcase (car a)) (downcase (car b)))))
               "")))

(defun condorit-classad-memory-used (ad)
  "Return the peak memory job AD used in MiB, or nil if not known.
This is condor's MemoryUsage, which -json prints only as its
expression: ResidentSetSize (KiB) rounded up to MiB."
  (let ((rss (plist-get ad :ResidentSetSize)))
    (and (numberp rss) (> rss 0) (ceiling rss 1024.0))))

(defun condorit-classad-failed-p (ad)
  "Return non-nil if AD completed with a non-zero exit code or a signal."
  (and (eq (condorit-classad-state ad) 'completed)
       (or (plist-get ad :ExitBySignal)
           (let ((code (plist-get ad :ExitCode)))
             (and (numberp code) (/= code 0))))))

(defun condorit-classad-machine-ad (slots)
  "Return the slot of SLOTS that describes the whole machine.
That is its partitionable or first static slot; dynamic slots are
carved out of it."
  (or (seq-find (lambda (slot) (not (equal (plist-get slot :SlotType) "Dynamic"))) slots)
      (car slots)))

(defun condorit-classad-machine-summary (slots)
  "Summarize one machine's SLOTS.
Return (:free :total :gpus-free :gpus-total :jobs :load :memory :disk
:draining); memory is in MiB and disk in KiB, as condor reports them."
  (let ((free 0) (total 0) (gpus-free 0) (gpus-total 0) (jobs 0) (load 0.0)
        (memory 0) (disk 0) draining)
    (dolist (slot slots)
      (let ((cpus (or (plist-get slot :Cpus) 0))
            (gpus (or (plist-get slot :GPUs) 0))
            (claimed (equal (plist-get slot :State) "Claimed"))
            (whole (lambda (own machine)
                     (let ((value (or (plist-get slot machine) (plist-get slot own))))
                       (if (numberp value) value 0)))))
        ;; Dynamic slots may report -1 (not measured).
        (let ((slot-load (plist-get slot :LoadAvg)))
          (when (and (numberp slot-load) (>= slot-load 0))
            (cl-incf load slot-load)))
        (when (eq (plist-get slot :Draining) t)
          (setq draining (or (plist-get slot :DrainReason) t)))
        (pcase (plist-get slot :SlotType)
          ("Partitionable"
           (cl-incf total (or (plist-get slot :TotalSlotCpus) cpus))
           (cl-incf free cpus)
           (cl-incf gpus-total (or (plist-get slot :TotalSlotGPUs) gpus))
           (cl-incf gpus-free gpus))
          ("Dynamic"
           (when claimed (cl-incf jobs)))
          (_
           (cl-incf total cpus)
           (cl-incf gpus-total gpus)
           (if claimed (cl-incf jobs) (cl-incf free cpus) (cl-incf gpus-free gpus))))
        (unless (equal (plist-get slot :SlotType) "Dynamic")
          (cl-incf memory (funcall whole :Memory :TotalSlotMemory))
          (cl-incf disk (funcall whole :Disk :TotalSlotDisk)))))
    (list :free free :total total :gpus-free gpus-free :gpus-total gpus-total
          :jobs jobs
          ;; Condor's own machine-wide load, else the sum over the slots.
          :load (let ((machine-load (plist-get (condorit-classad-machine-ad slots) :TotalLoadAvg)))
                  (if (numberp machine-load) machine-load load))
          :memory memory :disk disk :draining draining)))

(defun condorit-classad-group-machines (slots)
  "Group SLOTS by Machine into ((MACHINE . SLOTS) ...), sorted by name."
  (sort (seq-group-by (lambda (slot) (or (plist-get slot :Machine) "?")) slots)
        (lambda (a b) (string< (car a) (car b)))))

(defun condorit-classad-pool-summary (slots)
  "Summarize condor_status SLOTS for the whole pool.
Return (:machines :cpus-total :cpus-used :jobs :gpus-total :gpus-used),
added up from `condorit-classad-machine-summary'."
  (let* ((summaries (mapcar (lambda (machine) (condorit-classad-machine-summary (cdr machine)))
                            (condorit-classad-group-machines slots)))
         (sum (lambda (key) (apply #'+ (mapcar (lambda (s) (plist-get s key)) summaries)))))
    (list :machines (length summaries)
          :cpus-total (funcall sum :total)
          :cpus-used (- (funcall sum :total) (funcall sum :free))
          :jobs (funcall sum :jobs)
          :gpus-total (funcall sum :gpus-total)
          :gpus-used (- (funcall sum :gpus-total) (funcall sum :gpus-free)))))

(defun condorit-classad--format-disk (kib)
  "Format KIB kilobytes (the unit of Disk and RequestDisk) like memory."
  (condorit-classad-format-memory (/ kib 1024.0)))

(defconst condorit-classad--resources
  '((:RequestCpus :Cpus :TotalSlotCpus "CPUs" number-to-string)
    (:RequestMemory :Memory :TotalSlotMemory "memory" condorit-classad-format-memory)
    (:RequestDisk :Disk :TotalSlotDisk "disk" condorit-classad--format-disk))
  "(REQUEST OWN TOTAL NAME FORMAT) for each resource a job requests of a slot.
OWN and TOTAL are the slot attributes; TOTAL is the whole machine for
partitionable slots.")

(defun condorit-classad-slot-capacities (slots)
  "Return the distinct capacities one job could get from SLOTS.
Each is a list with one number per entry of `condorit-classad--resources',
or nil where the slot does not say."
  (delete-dups
   (mapcar (lambda (slot)
             (mapcar (pcase-lambda (`(,_ ,own ,total . ,_))
                       (let ((values (seq-filter #'numberp (list (plist-get slot own)
                                                                 (plist-get slot total)))))
                         (and values (apply #'max values))))
                     condorit-classad--resources))
           slots)))

(defun condorit-classad-misfit (job capacities)
  "Return why JOB fits none of the slot CAPACITIES, or nil if one fits.
Only numeric requests count; an expression or a capacity the slot does
not state is taken to fit.  Empty CAPACITIES (nothing known) fit."
  (let ((requests (mapcar (lambda (resource) (plist-get job (car resource)))
                          condorit-classad--resources))
        (fits (lambda (request capacity)
                (or (not (numberp request)) (null capacity) (<= request capacity)))))
    (unless (or (null capacities)
                (seq-some (lambda (capacity) (seq-every-p #'identity
                                                          (cl-mapcar fits requests capacity)))
                          capacities))
      (let ((too-big
             (cl-loop for (_ _ _ name format) in condorit-classad--resources
                      for i from 0
                      for request = (nth i requests)
                      for sizes = (mapcar (lambda (capacity) (nth i capacity)) capacities)
                      for largest = (and (not (memq nil sizes)) (seq-max sizes))
                      unless (funcall fits request largest)
                      collect (format "%s %s > %s" name
                                      (funcall format request) (funcall format largest)))))
        (if too-big
            (string-join too-big ", ")
          "no single slot has all the requested CPUs, memory and disk")))))

(defun condorit-classad--split-indexed (ad)
  "Split AD with numbered attributes (Name1, Name2, ...) into one ad per number.
This is the shape of condor_userprio -allusers -long output."
  (let ((table (make-hash-table)))
    (while ad
      (let ((key (symbol-name (car ad))))
        (when (string-match "\\`:\\(.*[A-Za-z_]\\)\\([0-9]+\\)\\'" key)
          (let ((n (string-to-number (match-string 2 key))))
            (puthash n (append (gethash n table)
                               (list (intern (concat ":" (match-string 1 key))) (cadr ad)))
                     table))))
      (setq ad (cddr ad)))
    (mapcar (lambda (n) (gethash n table))
            (sort (hash-table-keys table) #'<))))

(defun condorit-classad-parse-userprio (string)
  "Parse condor_userprio -long STRING into one plist per submitter.
Handles both the plain format (one ad per user) and the -allusers
format (a single ad with numbered attributes and NumSubmittors)."
  (let ((ads (condorit-classad-parse-long string)))
    (if (and (= (length ads) 1) (plist-member (car ads) :NumSubmittors))
        (condorit-classad--split-indexed (car ads))
      ads)))

(provide 'condorit-classad)
;;; condorit-classad.el ends here
