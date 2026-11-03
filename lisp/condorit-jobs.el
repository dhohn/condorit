;;; condorit-jobs.el --- Job lists for condorit  -*- lexical-binding: t; -*-

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
;; What the views that list jobs (status and history) share: the
;; parent mode `condorit-jobs-mode', job and cluster rows, finding the
;; jobs at point, and explaining the job at point in the echo area.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'condorit-classad)
(require 'condorit-faces)
(require 'condorit-base)

;;;; Jobs of this buffer

(defvar-local condorit-jobs--by-id nil "Hash table: job id string -> job plist.")

(defcustom condorit-memory-warning 0.9
  "Fraction of its requested memory from which a job's usage is highlighted.
Condor holds a job that uses more than it requested (hold code 34)."
  :type 'number
  :group 'condorit)

(defvar-local condorit-jobs--capacities nil
  "Slot capacities of the pool, from `condorit-classad-slot-capacities'.
Idle jobs that fit none of them are flagged; nil flags nothing.")

(defun condorit-jobs--index (jobs)
  "Make JOBS findable by id in this buffer; later JOBS win."
  (setq condorit-jobs--by-id (make-hash-table :test #'equal))
  (dolist (job jobs)
    (puthash (condorit-classad-job-id job) job condorit-jobs--by-id)))

;;;; Extra columns

(defcustom condorit-job-columns nil
  "Extra job attributes shown as columns, before TIME / DETAIL.
Each entry is (ATTRIBUTE TITLE WIDTH [ALIGN [FORMAT]]).  ALIGN `right'
suits numbers.  FORMAT, if given, turns the attribute's value (nil when
the job has none) into the cell text.  For example:

  (setq condorit-job-columns
        \='((\"RemoteHost\" \"HOST\" 36)
          (\"JobPrio\" \"PRIO\" 4 right)
          (\"RemoteWallClockTime\" \"WALL\" 6 right
           condorit-classad-format-duration)))

A cluster row shows the value its jobs share, or \"…\" if they differ.
The attributes are fetched from the next refresh on."
  :type '(repeat (list (string :tag "Attribute")
                       (string :tag "Title")
                       (natnum :tag "Width")
                       (choice :tag "Align" (const :tag "Left" nil) (const right))
                       (choice :tag "Format" (const :tag "As is" nil) function)))
  :group 'condorit)

(defun condorit-jobs-attributes ()
  "Return the job attributes to fetch, joined with commas for -attributes."
  (string-join (seq-uniq (append condorit-classad-job-attributes
                                 (mapcar #'car condorit-job-columns)))
               ","))

(defun condorit-jobs--columns ()
  "Return the columns of job rows: ID, BATCH, STATE, extras, TIME / DETAIL."
  (append (butlast condorit--job-columns)
          (mapcar (pcase-lambda (`(,_ ,title ,width ,align . ,_)) (list title width align))
                  condorit-job-columns)
          (last condorit--job-columns)))

(defun condorit-jobs--value (job column)
  "Return JOB's value for COLUMN of `condorit-job-columns' as cell text."
  (pcase-let* ((`(,attribute ,_ ,_ ,_ ,format) column)
               (key (intern (concat ":" attribute)))
               (value (plist-get job key)))
    (cond (format (or (funcall format value) ""))
          ((not (plist-member job key)) "")
          ((and (stringp value) (not (string-prefix-p "/Expr(" value))) value)
          (t (condorit-classad-format-value value)))))

(defun condorit-jobs--extra-cells (jobs)
  "Return the extra column cells for JOBS: a shared value, else \"…\"."
  (mapcar (lambda (column)
            (let ((values (delete-dups (mapcar (lambda (job) (condorit-jobs--value job column))
                                               jobs))))
              (if (cdr values) "…" (car values))))
          condorit-job-columns))

;;;; Sorting

(defvar-local condorit-jobs--sort nil
  "How this buffer sorts its jobs: (KEY . DESCENDING), or nil for by id.
KEY is a name from `condorit-jobs--sort-keys'.")

(defun condorit-jobs--state-time (job)
  "Return when JOB entered its current state, e.g. started running."
  (let ((start (plist-get job :JobCurrentStartDate))
        (completed (plist-get job :CompletionDate)))
    (pcase (condorit-classad-state job)
      ((and 'running (guard (numberp start))) start)
      ((and 'completed (guard (and (numberp completed) (> completed 0)))) completed)
      (_ (plist-get job :EnteredCurrentStatus)))))

(defun condorit-jobs--id-value (job)
  "Return (CLUSTER PROC) of JOB, to sort by."
  (list (plist-get job :ClusterId) (or (plist-get job :ProcId) 0)))

(defun condorit-jobs--shown-memory (job)
  "Return JOB's peak memory if its row shows it, else nil."
  (and (condorit-jobs--memory-shown-p job) (condorit-classad-memory-used job)))

(defun condorit-jobs--sort-keys ()
  "Return the sort keys: ((NAME . FUNCTION) ...); FUNCTION gives a job's value.
The columns of `condorit-job-columns' are keys too, by their title."
  (append '(("id" . condorit-jobs--id-value)
            ("name" . condorit-classad-batch-name)
            ("time" . condorit-jobs--state-time)
            ("memory" . condorit-jobs--shown-memory))
          (mapcar (lambda (column)
                    (let ((key (intern (concat ":" (car column)))))
                      (cons (downcase (nth 1 column)) (lambda (job) (plist-get job key)))))
                  condorit-job-columns)))

(defun condorit-jobs--before-p (a b descending)
  "Return non-nil if sort value A goes before B, DESCENDING or not.
Nil values go last; lists compare element by element."
  (cond ((null b) (and a t))
        ((null a) nil)
        ((and (consp a) (consp b))
         (if (equal (car a) (car b))
             (condorit-jobs--before-p (cdr a) (cdr b) descending)
           (condorit-jobs--before-p (car a) (car b) descending)))
        ((and (numberp a) (numberp b)) (if descending (> a b) (< a b)))
        (t (let ((a (format "%s" a)) (b (format "%s" b)))
             (if descending (string< b a) (string< a b))))))

(defun condorit-jobs--sort-jobs (jobs)
  "Return JOBS sorted the way this buffer sorts, stable within equal values."
  (pcase-let ((`(,key . ,descending) condorit-jobs--sort))
    (if-let* ((value (cdr (assoc key (condorit-jobs--sort-keys)))))
        (sort (copy-sequence jobs)
              (lambda (a b) (condorit-jobs--before-p (funcall value a) (funcall value b)
                                                    descending)))
      jobs)))

(defun condorit-jobs--group-clusters (jobs)
  "Group JOBS by cluster into ((ID . JOBS) ...), both levels sorted.
ID is the cluster's id string, see `condorit-classad-cluster-id'.
By id, or as `condorit-jobs--sort' says: then a cluster goes where its
first job does."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (job jobs)
      (push job (gethash (condorit-classad-cluster-id job) table)))
    (let ((by-id (sort (cl-loop for id being the hash-keys of table using (hash-values cjobs)
                                collect (cons id (sort cjobs (lambda (a b)
                                                               (< (plist-get a :ProcId)
                                                                  (plist-get b :ProcId))))))
                       (lambda (a b) (< (plist-get (cadr a) :ClusterId)
                                        (plist-get (cadr b) :ClusterId))))))
      (if (not condorit-jobs--sort)
          by-id
        (let ((sorted (mapcar (lambda (group)
                                (cons (car group) (condorit-jobs--sort-jobs (cdr group))))
                              by-id)))
          (mapcar (lambda (first) (assoc (condorit-classad-cluster-id first) sorted))
                  (condorit-jobs--sort-jobs (mapcar #'cadr sorted))))))))

(defun condorit-jobs--insert-sort-line ()
  "Insert a line saying how the jobs are sorted, unless by id."
  (when condorit-jobs--sort
    (insert (format "%s %s %s\n" (condorit--label "Sorted:") (car condorit-jobs--sort)
                    (if (cdr condorit-jobs--sort) "↓" "↑")))))

(defun condorit-jobs-sort (key)
  "Sort the job lists of this view by KEY; the same KEY again reverses them.
Interactively the current key is the default, so RET reverses.
KEY is id (the default), name, time (when the job entered its state),
memory (peak usage) or the title of a column in `condorit-job-columns'.
Clusters go where their first job does."
  (interactive
   (let ((current (or (car condorit-jobs--sort) "id")))
     (list (completing-read (format "Sort jobs by (RET reverses %s): " current)
                            (mapcar #'car (condorit-jobs--sort-keys)) nil t nil nil current))))
  (let ((descending (if (equal key (or (car condorit-jobs--sort) "id"))
                        (not (cdr condorit-jobs--sort))
                      ;; Ids and names ascending; times, sizes and numbers largest first.
                      (not (member key '("id" "name"))))))
    ;; Ascending by id is the default, kept as nil.
    (setq condorit-jobs--sort (and (or descending (not (equal key "id")))
                                  (cons key descending))))
  (condorit--render))

;;;; Summaries

(defun condorit-jobs--state-counts (jobs)
  "Return per-state counts of JOBS, e.g. \"R 92  I 30  H 3\"."
  (string-join
   (cl-loop for state in condorit--legend-order
            for n = (cl-count state jobs :key #'condorit-classad-state)
            when (> n 0)
            collect (condorit--propertize (format "%s %d" (condorit-classad-state-letter state) n)
                                         (condorit-state-face state)))
   "  "))

(defun condorit-jobs--longest-running (jobs now)
  "Return how long the longest-running job of JOBS has run at NOW, or nil."
  (let ((starts (delq nil (mapcar (lambda (job)
                                    (let ((start (plist-get job :JobCurrentStartDate)))
                                      (and (eq (condorit-classad-state job) 'running)
                                           (numberp start)
                                           start)))
                                  jobs))))
    (and starts (- now (apply #'min starts)))))

;;;; Rows

(defconst condorit-jobs--hold-labels
  '((12 . "output transfer") (13 . "input transfer"))
  "Short labels for hold codes whose message alone does not say what failed.")

(defun condorit-jobs--reason-detail (reason)
  "Return the informative part of the hold REASON message.
Condor names the failing file and the system error in the middle of
the message (9.0) or at its end (25.0); prefer those.  Otherwise drop
the \"Error from SLOT: \" prefix, which says nothing about the cause."
  (cond ((string-match "\\([^ :;]+\\): (errno [0-9]+) \\([^;]+\\)" reason)
         (let ((file (match-string 1 reason))
               (error (string-trim (match-string 2 reason))))
           (format "%s: %s" (file-name-nondirectory file) error)))
        ((string-match "\\`Error from [^ ]+: " reason)
         (substring reason (match-end 0)))
        (t reason)))

(defun condorit-jobs--short-reason (job)
  "Return the HoldReason of JOB, shortened to the part naming the cause."
  (let ((reason (plist-get job :HoldReason)))
    (if (stringp reason)
        (let ((detail (condorit-jobs--reason-detail (car (split-string reason "\n"))))
              (label (alist-get (plist-get job :HoldReasonCode) condorit-jobs--hold-labels)))
          (condorit--propertize
           (truncate-string-to-width (if label (concat label ": " detail) detail)
                                     70 nil nil "…")
           'condorit-hold-reason))
      "no hold reason given")))

(defun condorit-jobs--since (time now)
  "Return how long ago TIME was at NOW, in the number face, or \"\"."
  (if (and (numberp time) (> time 0))
      (condorit--propertize (condorit-classad-format-duration (- now time)) 'condorit-number)
    ""))

(defun condorit-jobs--finished-ago (job now)
  "Return how long ago JOB left the queue at NOW, e.g. \"2h05m ago\"."
  (let* ((completed (plist-get job :CompletionDate))
         (time (if (and (numberp completed) (> completed 0))
                   completed
                 (plist-get job :EnteredCurrentStatus)))
         (since (condorit-jobs--since time now)))
    (if (string-empty-p since) "" (concat since " ago"))))

(defun condorit-jobs--misfit (job)
  "Return why idle JOB fits no slot of the pool, or nil."
  (and (eq (condorit-classad-state job) 'idle)
       (condorit-classad-misfit job condorit-jobs--capacities)))

(defun condorit-jobs--memory-shown-p (job)
  "Return non-nil if JOB's row shows its memory: it runs or completed."
  (memq (condorit-classad-state job) '(running completed)))

(defun condorit-jobs--memory (jobs)
  "Return the peak memory of JOBS against their request, e.g. \"mem 3.8G/4.0G\".
Both are the largest among JOBS.  The text is in the warning face from
`condorit-memory-warning' of the request on.  Nil if no job reports usage."
  (let ((used (seq-max (cons 0 (delq nil (mapcar #'condorit-classad-memory-used jobs)))))
        (requested (seq-max (cons 0 (seq-filter #'numberp (mapcar (lambda (job)
                                                                    (plist-get job :RequestMemory))
                                                                  jobs))))))
    (when (> used 0)
      (condorit--propertize
       (if (> requested 0)
           (format "mem %s/%s" (condorit-classad-format-memory used)
                   (condorit-classad-format-memory requested))
         (format "mem %s" (condorit-classad-format-memory used)))
       (if (and (> requested 0) (>= used (* condorit-memory-warning requested)))
           'condorit-hold-reason
         'condorit-number)))))

(defun condorit-jobs--detail (job now)
  "Return the trailing detail shown for JOB at NOW."
  (pcase (condorit-classad-state job)
    ('running
     (string-join (delete "" (list (condorit-jobs--since (plist-get job :JobCurrentStartDate) now)
                                   (or (condorit-jobs--memory (list job)) "")))
                  " · "))
    ('idle
     (let ((since (condorit-jobs--since (plist-get job :EnteredCurrentStatus) now))
           (misfit (condorit-jobs--misfit job)))
       (if misfit
           (string-join (delete "" (list since (condorit--propertize
                                                (concat "✗ fits no slot: " misfit)
                                                'condorit-failed)))
                        " · ")
         since)))
    ('held (condorit-jobs--short-reason job))
    ('completed
     (string-join
      (delete "" (list (if (condorit-classad-failed-p job)
                           (condorit--propertize
                            (format "✗ exit %s" (or (plist-get job :ExitCode) "by signal"))
                            'condorit-failed)
                         "")
                       (condorit-jobs--finished-ago job now)
                       (or (condorit-jobs--memory (list job)) "")))
      " · "))
    ('removed
     (string-join (delete "" (list (condorit-jobs--finished-ago job now)
                                   (or (plist-get job :RemoveReason) "")))
                  " · "))
    (_ "")))

(defun condorit-jobs--insert-job (job indent &optional detail nested)
  "Insert a one-line section for JOB, indented by INDENT.
NESTED non-nil means JOB is shown under its cluster: its id is
indented within the ID column and the batch name is left out.
DETAIL is the text of the TIME/DETAIL column."
  (let ((state (condorit-classad-state job)))
    (magit-insert-section (condorit-job (condorit-classad-job-id job))
      (magit-insert-heading
        (condorit--row indent (condorit-jobs--columns)
                      (append
                       (list (concat (if nested "  " "")
                                     (condorit--propertize
                                      (format "%s.%s" (plist-get job :ClusterId)
                                              (plist-get job :ProcId))
                                      'condorit-id))
                             (if nested "" (condorit-classad-batch-name job))
                             (condorit--propertize (condorit-classad-state-letter state)
                                                  (condorit-state-face state)))
                       (condorit-jobs--extra-cells (list job))
                       (list (or detail ""))))))))

(defun condorit-jobs--cluster-heading (jobs now indent finished)
  "Return the heading row of the cluster of JOBS, indented by INDENT.
NOW is the current time; FINISHED non-nil means the jobs have left
the queue, so no running time is shown."
  (let ((running (and (not finished) (condorit-jobs--longest-running jobs now)))
        (failed (cl-count-if #'condorit-classad-failed-p jobs))
        (misfits (cl-count-if #'condorit-jobs--misfit jobs))
        (memory (condorit-jobs--memory (seq-filter #'condorit-jobs--memory-shown-p jobs))))
    (condorit--row indent (condorit-jobs--columns)
                  (append
                   (list (condorit--propertize (number-to-string (plist-get (car jobs) :ClusterId))
                                              'condorit-id)
                         (condorit-classad-batch-name (car jobs))
                         (condorit-jobs--state-counts jobs))
                   (condorit-jobs--extra-cells jobs)
                   (list
                    (string-join
                     (delq nil (list (and running
                                          (condorit--propertize
                                           (condorit-classad-format-duration running)
                                           'condorit-number))
                                     (and (> failed 0)
                                          (condorit--propertize (format "✗ %d failed" failed)
                                                               'condorit-failed))
                                     (and (> misfits 0)
                                          (condorit--propertize
                                           (format "✗ %d too big for any slot" misfits)
                                           'condorit-failed))
                                     memory))
                     " · "))))))

(defun condorit-jobs--insert-clusters (jobs now indent finished)
  "Insert collapsed cluster sections for JOBS, indented by INDENT.
NOW is the current time.  FINISHED non-nil means the jobs have left
the queue."
  (pcase-dolist (`(,id . ,cjobs) (condorit-jobs--group-clusters jobs))
    (magit-insert-section (condorit-cluster id t)
      (magit-insert-heading
        (condorit-jobs--cluster-heading cjobs now indent finished))
      ;; Only build the procs when the cluster is expanded: inserting
      ;; thousands of job sections up front makes every refresh slow.
      (magit-insert-section-body
        (dolist (job cjobs)
          (condorit-jobs--insert-job job indent (condorit-jobs--detail job now) t))))))

;;;; Jobs at point

(defun condorit-jobs--normalize-ids (ids)
  "Remove duplicates from IDS and procs whose cluster is also in IDS."
  (let ((ids (delete-dups (copy-sequence ids))))
    (seq-remove (lambda (id)
                  (and (string-match "\\`\\(\\(?:[^#]+#\\)?[0-9]+\\)\\.[0-9]+\\'" id)
                       (member (match-string 1 id) ids)))
                ids)))

(defun condorit-jobs--ids-constraint (ids)
  "Return a ClassAd expression matching job or cluster IDS of one schedd.
IDS do not name the schedd; see `condorit-jobs--split-ids'."
  (mapconcat (lambda (id)
               (if (string-match "\\`\\([0-9]+\\)\\.\\([0-9]+\\)\\'" id)
                   (format "(ClusterId == %s && ProcId == %s)"
                           (match-string 1 id) (match-string 2 id))
                 (format "ClusterId == %s" id)))
             ids " || "))

(defun condorit-jobs--split-ids (ids)
  "Return (SCHEDD . LOCAL-IDS) for job or cluster IDS of one schedd.
SCHEDD is nil for the schedd of this buffer.  Signal a `user-error' if
IDS belong to several schedds."
  (let ((split (mapcar #'condorit-classad-split-id ids)))
    (when (cdr (delete-dups (mapcar #'car split)))
      (user-error "Select jobs of one schedd at a time"))
    (cons (caar split) (mapcar #'cdr split))))

(defun condorit-jobs--schedd-args (schedd)
  "Return the arguments naming SCHEDD for a condor command, or nil."
  (and schedd (list "-name" schedd)))

(defun condorit-jobs--queued-of-ids (ids)
  "Return the queued jobs of this buffer that IDS name, as jobs or clusters."
  (seq-filter (lambda (job)
                (or (member (condorit-classad-job-id job) ids)
                    (member (condorit-classad-cluster-id job) ids)))
              (plist-get condorit--data :jobs)))

(defun condorit-jobs--others (jobs)
  "Return the owners of JOBS other than this buffer's user, sorted."
  (sort (delete condorit--user (delete-dups (delq nil (mapcar (lambda (job)
                                                                (plist-get job :Owner))
                                                              jobs))))
        #'string<))

(defun condorit-jobs--describe-ids (ids)
  "Describe job or cluster IDS for a confirmation prompt.
For example \"1240 on a.example.org (2000 jobs, 92 running, of bob)\":
the schedd if it is another one, and what this buffer knows of the
jobs: how many, how many run, and whose they are if not yours."
  (pcase-let* ((`(,schedd . ,local) (condorit-jobs--split-ids ids))
               (jobs (condorit-jobs--queued-of-ids ids))
               (running (cl-count 'running jobs :key #'condorit-classad-state))
               (others (condorit-jobs--others jobs)))
    (concat (string-join local " ")
            (if schedd (concat " on " schedd) "")
            (if jobs
                (format " (%s)"
                        (string-join
                         (delq nil (list (format "%d job%s" (length jobs)
                                                 (if (cdr jobs) "s" ""))
                                         (and (> running 0) (format "%d running" running))
                                         (and others (concat "of " (string-join others ", ")))))
                         ", "))
              ""))))

(defun condorit-jobs-at-point ()
  "Return the ids of the selected jobs and clusters, or the one at point."
  (let* ((sections (if (region-active-p)
                       (or (magit-region-sections '(condorit-job condorit-cluster))
                           (user-error "Select rows at the same level (e.g. only clusters or only jobs of one cluster)"))
                     (and (bound-and-true-p magit-root-section)
                          (list (magit-current-section)))))
         (ids (cl-loop for section in sections
                       when (memq (oref section type) '(condorit-job condorit-cluster))
                       collect (oref section value))))
    (or (condorit-jobs--normalize-ids ids)
        (user-error "No job at point"))))

(defun condorit-job-id-at-point ()
  "Return the id of the one job or cluster selected or at point.
Signal a `user-error' when several are selected."
  (let ((ids (condorit-jobs-at-point)))
    (when (cdr ids)
      (user-error "This works on one job or cluster at a time; %d are selected"
                  (length ids)))
    (car ids)))

(defun condorit-jobs--first-proc-id (cluster)
  "Return the id of the lowest proc of CLUSTER (a string) known in this buffer."
  (when condorit-jobs--by-id
    (let (best)
      (maphash (lambda (_id job)
                 (when (and (equal (condorit-classad-cluster-id job) cluster)
                            (or (null best)
                                (< (plist-get job :ProcId) (plist-get best :ProcId))))
                   (setq best job)))
               condorit-jobs--by-id)
      (and best (condorit-classad-job-id best)))))

(defun condorit-job-at-point ()
  "Return the job plist at point; on a cluster heading, its first proc."
  (let* ((section (and (bound-and-true-p magit-root-section) (magit-current-section)))
         (id (pcase (and section (oref section type))
               ('condorit-job (oref section value))
               ('condorit-cluster (condorit-jobs--first-proc-id (oref section value)))
               (_ (user-error "No job at point")))))
    (or (and id condorit-jobs--by-id (gethash id condorit-jobs--by-id))
        (user-error (substitute-command-keys "No data for job %s; refresh with \\[condorit-refresh]")
                    id))))

;;;; Explanations

(defconst condorit-jobs--hold-reasons
  '((1  "You (or an admin) put the job on hold with condor_hold."
        "Release it with \\[condorit-release] when ready.")
    (3  "A periodic_hold or on_exit_hold expression of the job became true."
        "Check the hold expressions in the submit file, then release with \\[condorit-release].")
    (6  "The job's executable could not be started."
        "Check the executable path and permissions; open the event log with \\[condorit-logs] l.")
    (7  "An output file could not be opened."
        "Check that the output directory exists and is writable.")
    (8  "An input file could not be opened."
        "Check that the input files named in the submit file exist.")
    (12 "Condor failed to receive or write the job's files during a transfer."
        "Check that the destination directory exists and has space; see the event log with \\[condorit-logs] l.")
    (13 "Condor failed to read or send the job's files during a transfer."
        "Check that every file in transfer_input_files exists; see the event log with \\[condorit-logs] l.")
    (14 "The job's initial working directory (Iwd) is not accessible."
        "Check that the directory still exists.")
    (15 "The job was submitted on hold (hold = true in the submit file)."
        "Release it with \\[condorit-release] when ready.")
    (21 "The execute node's policy (WANT_HOLD) put the job on hold."
        "Read the hold reason; it usually names the cause.")
    (22 "The job event log (UserLog) could not be written."
        "Check that the log directory exists and is writable.")
    (26 "A system-wide hold policy of the admins matched the job."
        "Read the hold reason; ask your admins if it is unclear.")
    (32 "The input files exceed the allowed transfer size."
        "Reduce the input or raise MaxTransferInputMB.")
    (33 "The output files exceed the allowed transfer size."
        "Reduce the output or raise MaxTransferOutputMB.")
    (34 "The job used more of a resource (usually memory) than it requested."
        "Raise the request (e.g. RequestMemory) with \\[condorit-edit], then release with \\[condorit-release]."))
  "HoldReasonCode → (EXPLANATION HINT), checked against the HTCondor 9.0 manual.
HINT names keys as `substitute-command-keys' does.")

(defun condorit-jobs--hold-explanation (code)
  "Return (EXPLANATION HINT) for HoldReasonCode CODE, or nil if unknown."
  (and (integerp code) (cdr (assq code condorit-jobs--hold-reasons))))

(defun condorit-jobs--memory-advice (job)
  "Return a sentence on JOB's peak memory against its request, or \"\"."
  (let ((used (condorit-classad-memory-used job))
        (requested (plist-get job :RequestMemory)))
    (if (and used (numberp requested) (> requested 0))
        (let ((share (/ (* 100.0 used) requested)))
          (format "  Peak memory %s of %s requested (%d%%)%s"
                  (condorit-classad-format-memory used)
                  (condorit-classad-format-memory requested)
                  (round share)
                  (cond ((>= share (* 100 condorit-memory-warning))
                         (if (eq (condorit-classad-state job) 'running)
                             (substitute-command-keys
                              ": condor holds it if it goes over; raise the request with \\[condorit-edit].")
                           ": request more next time."))
                        ((and (< share 25) (> (- requested used) 1024))
                         ": a smaller request_memory would match sooner.")
                        (t "."))))
      "")))

(defun condorit-jobs--explain (job now)
  "Return a sentence explaining JOB's state at NOW and what to do next."
  (pcase (condorit-classad-state job)
    ('held
     (if-let* ((gloss (condorit-jobs--hold-explanation (plist-get job :HoldReasonCode))))
         (format "Held: %s %s" (car gloss) (substitute-command-keys (cadr gloss)))
       (format (substitute-command-keys "Held: %s  Fix the cause, then release with \\[condorit-release].")
               (or (plist-get job :HoldReason) "no reason given."))))
    ('idle
     (if-let* ((misfit (condorit-jobs--misfit job)))
         (format (substitute-command-keys
                  "Idle, and no slot of the pool is big enough (%s).  Lower the request with \\[condorit-edit].")
                 misfit)
       (substitute-command-keys
        "Idle: its requests fit some slot, but its Requirements may still match none.  Press \\[condorit-analyze] to see why it isn't running yet.")))
    ('running
     (concat (format "Running for %s on %s."
                     (condorit-classad-format-duration
                      (- now (or (plist-get job :JobCurrentStartDate) now)))
                     (or (plist-get job :RemoteHost) "an execute node"))
             (condorit-jobs--memory-advice job)))
    ('completed
     (if (condorit-classad-failed-p job)
         (format (substitute-command-keys "Completed with %s.  Check stderr with \\[condorit-logs] e.")
                 (if (plist-get job :ExitBySignal)
                     "a signal"
                   (format "exit code %s" (plist-get job :ExitCode))))
       (concat "Completed successfully (exit code 0)."
               (condorit-jobs--memory-advice job))))
    ('removed "Removed (by condor_rm or a periodic_remove policy).")
    ('transferring "Transferring output files back to the submit machine.")
    ('suspended "Suspended by the execute node; it resumes on its own.")))

(defun condorit-jobs--count-with-time (n state seconds)
  "Describe N jobs in STATE, the longest for SECONDS, e.g. \"2 idle, longest 3h\"."
  (let ((time (condorit-classad-format-duration seconds)))
    (cond ((string-empty-p time) (format "%d %s" n state))
          ((= n 1) (format "1 %s for %s" state time))
          (t (format "%d %s, longest %s" n state time)))))

(defun condorit-jobs--explain-cluster (cluster jobs now)
  "Return a sentence summing up the JOBS of CLUSTER (a number) at NOW."
  (let* ((by-state (seq-group-by #'condorit-classad-state jobs))
         (parts
          (cl-loop
           for state in condorit--legend-order
           for some = (alist-get state by-state)
           for n = (length some)
           when some
           collect
           (pcase state
             ('running (condorit-jobs--count-with-time
                        n "running" (condorit-jobs--longest-running some now)))
             ('idle (condorit-jobs--count-with-time
                     n "idle"
                     (let ((since (seq-filter #'numberp (mapcar (lambda (job)
                                                                  (plist-get job :EnteredCurrentStatus))
                                                                some))))
                       (and since (- now (apply #'min since))))))
             ('held (let ((reasons (delete-dups
                                    (mapcar (lambda (job)
                                              (substring-no-properties
                                               (condorit-jobs--short-reason job)))
                                            some))))
                      (if (cdr reasons)
                          (format "%d held, for %d different reasons" n (length reasons))
                        (format "%d held: %s" n (car reasons)))))
             ('completed (let ((failed (cl-count-if #'condorit-classad-failed-p some)))
                           (if (> failed 0)
                               (format "%d completed, %d of them failed" n failed)
                             (format "%d completed" n))))
             (_ (format "%d %s" n (downcase (condorit-classad-state-name state))))))))
    (format (substitute-command-keys "Cluster %d (%s): %s.  \\[magit-section-toggle] lists its jobs.")
            cluster (condorit-classad-batch-name (car jobs)) (string-join parts "; "))))

(defun condorit-jobs--schedd-note (job)
  "Return \"  (schedd NAME)\" if JOB is marked with its schedd, else \"\"."
  (if-let* ((schedd (plist-get job :condorit-schedd)))
      (format "  (schedd %s)" schedd)
    ""))

(defun condorit-jobs--eldoc (_callback &rest _)
  "Explain the job or cluster at point in the echo area."
  (when-let* (((bound-and-true-p magit-root-section))
              (section (magit-current-section))
              (jobs condorit-jobs--by-id))
    (pcase (oref section type)
      ('condorit-job
       (when-let* ((job (gethash (oref section value) jobs)))
         (concat (condorit-jobs--explain job (float-time))
                 (condorit-jobs--schedd-note job))))
      ('condorit-cluster
       (let ((cluster (oref section value))
             found)
         (maphash (lambda (_id job)
                    (when (equal (condorit-classad-cluster-id job) cluster)
                      (push job found)))
                  jobs)
         (when found
           (concat (condorit-jobs--explain-cluster (plist-get (car found) :ClusterId)
                                                   found (float-time))
                   (condorit-jobs--schedd-note (car found)))))))))

;;;; Mode

(declare-function condorit-visit "condorit-actions" ())
(declare-function condorit-logs "condorit-actions" (job))

(defvar-keymap condorit-jobs-mode-map
  :doc "Keys for the job at point, in every view that lists jobs."
  :parent condorit-mode-map
  "RET" #'condorit-visit
  "l"   #'condorit-logs
  "o"   #'condorit-jobs-sort)

(define-derived-mode condorit-jobs-mode condorit-mode "Condorit"
  "Parent mode of condorit buffers that list jobs."
  :interactive nil
  (add-hook 'eldoc-documentation-functions #'condorit-jobs--eldoc nil t)
  (eldoc-mode 1))

(provide 'condorit-jobs)
;;; condorit-jobs.el ends here
