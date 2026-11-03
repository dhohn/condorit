;;; condorit-pool.el --- The pool view of condorit  -*- lexical-binding: t; -*-

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
;; The pool view: the machines of the pool with their slots.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'condorit-process)
(require 'condorit-classad)
(require 'condorit-faces)
(require 'condorit-base)

(defcustom condorit-pool-site-attributes nil
  "Machine attributes of your site to show under each machine of the pool view.
Sites often advertise their own attributes, e.g. which container
runtimes a machine has: \\='(\"HasSingularity\" \"HasDocker\")."
  :type '(repeat string)
  :group 'condorit)

(defconst condorit-pool--attributes
  (append condorit-classad-slot-attributes
          '("GPUs" "TotalSlotGPUs" "CUDADeviceName" "CUDAGlobalMemoryMb"
            "OpSysLongName" "OpSysAndVer" "Arch" "has_avx512f" "CpuFamily" "CpuModelNumber"
            "Draining" "DrainReason" "JobStart" "TotalLoadAvg"))
  "Slot attributes the pool view asks condor_status for.
`condorit-pool-site-attributes' are added to these.")

(defconst condorit-pool--cpu-models
  '(;; Intel servers
    ((6 . 44) . "Intel Westmere-EP") ((6 . 45) . "Intel Sandy Bridge-EP")
    ((6 . 62) . "Intel Ivy Bridge-EP") ((6 . 63) . "Intel Haswell-EP")
    ((6 . 79) . "Intel Broadwell-EP") ((6 . 85) . "Intel Skylake-SP/Cascade Lake")
    ((6 . 106) . "Intel Ice Lake-SP") ((6 . 143) . "Intel Sapphire Rapids")
    ((6 . 207) . "Intel Emerald Rapids") ((6 . 173) . "Intel Granite Rapids")
    ((6 . 175) . "Intel Sierra Forest")
    ;; Intel desktops
    ((6 . 42) . "Intel Sandy Bridge") ((6 . 58) . "Intel Ivy Bridge")
    ((6 . 60) . "Intel Haswell") ((6 . 94) . "Intel Skylake")
    ((6 . 158) . "Intel Kaby/Coffee Lake") ((6 . 165) . "Intel Comet Lake")
    ((6 . 151) . "Intel Alder Lake") ((6 . 183) . "Intel Raptor Lake")
    ;; AMD
    ((23 . 1) . "AMD Naples (Zen)") ((23 . 49) . "AMD Rome (Zen 2)")
    ((23 . 113) . "AMD Matisse (Zen 2)") ((25 . 1) . "AMD Milan (Zen 3)")
    ((25 . 33) . "AMD Vermeer (Zen 3)") ((25 . 17) . "AMD Genoa (Zen 4)")
    ((25 . 97) . "AMD Raphael (Zen 4)") ((25 . 160) . "AMD Bergamo (Zen 4c)")
    ((26 . 2) . "AMD Turin (Zen 5)") ((26 . 17) . "AMD Turin Dense (Zen 5c)"))
  "CPU names of x86 (CpuFamily . CpuModelNumber) pairs.
Condor advertises only these CPUID numbers, not the model name.")

(defun condorit-pool--cpu-name (ad)
  "Return the CPU name of machine AD, or nil if AD has no CPUID numbers."
  (let ((family (plist-get ad :CpuFamily))
        (model (plist-get ad :CpuModelNumber)))
    (when (and (integerp family) (integerp model))
      (or (alist-get (cons family model) condorit-pool--cpu-models nil nil #'equal)
          (pcase family
            (6 (format "Intel model %d" model))
            ((or 23 25 26) (format "AMD family %d model %d" family model))
            (_ (format "CPU family %d model %d" family model)))))))

(defconst condorit-pool--machine-columns
  '(("MACHINE" 32) ("FREE CPUS" 10) ("FREE GPUS" 10) ("JOBS" 4 right) ("LOAD" 6 right)
    ("" nil))
  "Columns of machine rows.")

(defun condorit-pool--condor-args (options)
  "Return the condor_status words of view OPTIONS.
Options starting with \"--\" filter or group the machines in condorit
itself; condor_status never sees them."
  (condorit-expand-args (seq-remove (lambda (option) (string-prefix-p "--" option))
                                   options)))

(defun condorit-pool--commands ()
  "Return the commands a refresh of the pool view runs."
  `((slots "condor_status"
           ("-json" "-attributes"
            ,(string-join (append condorit-pool--attributes condorit-pool-site-attributes)
                          ",")
            ,@(condorit-pool--condor-args condorit-options)))))

(defun condorit-pool--digits (n)
  "Return how many characters N takes when printed."
  (length (number-to-string n)))

(defun condorit-pool--fraction-widths (summaries free total)
  "Return (FREE-WIDTH . TOTAL-WIDTH) that fit FREE/TOTAL of all SUMMARIES.
FREE and TOTAL are summary keys; summaries with a zero TOTAL are left out."
  (let ((shown (seq-filter (lambda (summary) (> (plist-get summary total) 0)) summaries)))
    (cons (seq-max (cons 0 (mapcar (lambda (s) (condorit-pool--digits (plist-get s free))) shown)))
          (seq-max (cons 0 (mapcar (lambda (s) (condorit-pool--digits (plist-get s total)))
                                   shown))))))

(defun condorit-pool--fraction (free total widths)
  "Format FREE of TOTAL as \"FREE/TOTAL\", aligned on the slash.
WIDTHS is (FREE-WIDTH . TOTAL-WIDTH)."
  (concat (string-pad (condorit--number free) (car widths) nil t)
          "/"
          (string-pad (condorit--number total) (cdr widths))))

(defun condorit-pool--machine-row (machine summary indent widths)
  "Return the heading row of MACHINE with SUMMARY, indented by INDENT.
WIDTHS is (:cpus WIDTHS :gpus WIDTHS), see `condorit-pool--fraction'."
  (let ((draining (plist-get summary :draining)))
    (condorit--row indent condorit-pool--machine-columns
                  (list machine
                        (condorit-pool--fraction (plist-get summary :free)
                                                (plist-get summary :total)
                                                (plist-get widths :cpus))
                        (if (> (plist-get summary :gpus-total) 0)
                            (condorit-pool--fraction (plist-get summary :gpus-free)
                                                    (plist-get summary :gpus-total)
                                                    (plist-get widths :gpus))
                          "")
                        (condorit--number (plist-get summary :jobs))
                        (condorit--propertize (format "%.1f" (plist-get summary :load))
                                             'condorit-number)
                        (if draining
                            (condorit--propertize "draining" 'condorit-hold-reason)
                          "")))))

(defun condorit-pool--gpu-text (ad gpus)
  "Describe the GPUS of machine AD, e.g. \"8× NVIDIA H200 (139.8G)\"."
  (let ((name (plist-get ad :CUDADeviceName))
        (memory (plist-get ad :CUDAGlobalMemoryMb)))
    (concat (format "%d× %s" gpus (if (stringp name) name "GPU"))
            (if (numberp memory)
                (format " (%s)" (condorit-classad-format-memory memory))
              ""))))

(defun condorit-pool--details (slots summary)
  "Return the detail lines of a machine with SLOTS and SUMMARY.
Each is (LABEL . TEXT); lines without content are left out."
  (let* ((ad (condorit-classad-machine-ad slots))
         (draining (plist-get summary :draining))
         (join (lambda (&rest parts) (string-join (delq nil parts) " · ")))
         (site (cl-loop for attribute in condorit-pool-site-attributes
                        for value = (plist-get ad (intern (concat ":" attribute)))
                        when (plist-member ad (intern (concat ":" attribute)))
                        collect (format "%s %s" attribute
                                        (if (stringp value)
                                            value
                                          (condorit-classad-format-value value))))))
    (seq-filter
     #'cdr
     (list (cons "Hardware"
                 (funcall join
                          (format "%d CPUs" (plist-get summary :total))
                          (condorit-classad-format-memory (plist-get summary :memory))
                          (and (> (plist-get summary :disk) 0)
                               (concat (condorit-classad--format-disk (plist-get summary :disk))
                                       " disk"))
                          (and (> (plist-get summary :gpus-total) 0)
                               (condorit-pool--gpu-text ad (plist-get summary :gpus-total)))))
           (cons "System"
                 (let ((os (or (plist-get ad :OpSysLongName) (plist-get ad :OpSysAndVer)))
                       (arch (plist-get ad :Arch)))
                   (and (or os arch)
                        (funcall join os arch (condorit-pool--cpu-name ad)
                                 (and (eq (plist-get ad :has_avx512f) t) "AVX-512")))))
           (cons "Site" (and site (string-join site " · ")))
           (cons "Status" (and draining
                               (condorit--propertize
                                (if (stringp draining) (concat "draining: " draining) "draining")
                                'condorit-hold-reason)))))))

(defconst condorit-pool--slot-columns
  '(("SLOT" 40) ("STATE" 14) ("CPUS" 8 right) ("MEMORY" 7 right) ("GPUS" 7 right) ("JOB" nil))
  "Columns of slot rows; they show no header.")

(defun condorit-pool--slot-line (slot now indent)
  "Return the line shown for SLOT under its machine at NOW, after INDENT."
  (let ((state (or (plist-get slot :State) "?"))
        (gpus (plist-get slot :GPUs))
        (start (plist-get slot :JobStart)))
    (condorit--row
     indent condorit-pool--slot-columns
     (list (plist-get slot :Name)
           (concat (condorit--propertize state (if (equal state "Claimed")
                                                  'condorit-slot-claimed
                                                'condorit-slot-unclaimed))
                   "/" (or (plist-get slot :Activity) ""))
           (format "%d CPUs" (or (plist-get slot :Cpus) 0))
           (condorit-classad-format-memory (plist-get slot :Memory))
           (if (and (numberp gpus) (> gpus 0)) (format "%d GPUs" gpus) "")
           (concat (or (plist-get slot :RemoteOwner) "")
                   (if (and (equal state "Claimed") (numberp start) (> start 0))
                       (concat " · " (condorit--propertize
                                      (condorit-classad-format-duration (- now start))
                                      'condorit-number))
                     ""))))))

(defun condorit-pool--runs-mine-p (slots user)
  "Return non-nil if one of SLOTS runs a job of USER."
  (let ((prefix (concat user "@")))
    (seq-some (lambda (slot)
                (let ((owner (plist-get slot :RemoteOwner)))
                  (and (stringp owner) (string-prefix-p prefix owner))))
              slots)))

(defun condorit-pool--filter (machines options user)
  "Return the MACHINES that pass the local filters of view OPTIONS.
MACHINES are (MACHINE SLOTS SUMMARY); USER is whose jobs \"--mine\" means."
  (seq-filter (pcase-lambda (`(,_ ,slots ,summary))
                (and (or (not (member "--free-cpus" options))
                         (> (plist-get summary :free) 0))
                     (or (not (member "--free-gpus" options))
                         (> (plist-get summary :gpus-free) 0))
                     (or (not (member "--mine" options))
                         (condorit-pool--runs-mine-p slots user))))
              machines))

(defun condorit-pool--group-key (slots grouping)
  "Return the group of the machine with SLOTS under GROUPING.
GROUPING is \"gpu\", \"cpu\" or \"os\"."
  (let ((ad (condorit-classad-machine-ad slots)))
    (pcase grouping
      ("gpu" (let ((name (plist-get ad :CUDADeviceName)))
               (if (stringp name) name "No GPU")))
      ("cpu" (or (condorit-pool--cpu-name ad) "?"))
      ("os" (or (plist-get ad :OpSysLongName) (plist-get ad :OpSysAndVer) "?")))))

(defun condorit-pool--grouping (options)
  "Return the grouping chosen in view OPTIONS (\"--group=...\"), or nil."
  (seq-some (lambda (option)
              (and (string-match "\\`--group=\\(.+\\)\\'" option) (match-string 1 option)))
            options))

(defun condorit-pool--insert-machines (machines now indent widths)
  "Insert folded sections for MACHINES, indented by INDENT, at NOW.
WIDTHS aligns the CPU and GPU columns, see `condorit-pool--machine-row'."
  (let ((inner (concat indent "  ")))
    (pcase-dolist (`(,machine ,mslots ,summary) machines)
      (magit-insert-section (condorit-machine machine t)
        (magit-insert-heading (condorit-pool--machine-row machine summary indent widths))
        (pcase-dolist (`(,label . ,text) (condorit-pool--details mslots summary))
          (insert (format "%s%s %s\n" inner
                          (condorit--propertize (format "%-9s" label) 'condorit-header-label)
                          text)))
        (dolist (slot mslots)
          (magit-insert-section (condorit-slot (plist-get slot :Name))
            (magit-insert-heading (condorit-pool--slot-line slot now inner))))))))

(defun condorit-pool--insert (data)
  "Insert the pool view's content from DATA."
  (let* ((slots (plist-get data :slots))
         (now (or (plist-get data :time) (float-time)))
         (all (mapcar (pcase-lambda (`(,machine . ,mslots))
                        (list machine mslots (condorit-classad-machine-summary mslots)))
                      (condorit-classad-group-machines slots)))
         (machines (condorit-pool--filter all condorit-options condorit--user))
         (grouping (condorit-pool--grouping condorit-options))
         (summaries (mapcar (lambda (machine) (nth 2 machine)) machines))
         (widths (list :cpus (condorit-pool--fraction-widths summaries :free :total)
                       :gpus (condorit-pool--fraction-widths summaries :gpus-free :gpus-total)))
         (pool (condorit-classad-pool-summary slots)))
    (insert (format "%s %s/%s CPUs busy%s · %s machine(s) · %s slot(s) claimed   %s\n"
                    (condorit--label "Pool:")
                    (condorit--number (plist-get pool :cpus-used))
                    (condorit--number (plist-get pool :cpus-total))
                    (if (> (plist-get pool :gpus-total) 0)
                        (format " · %s/%s GPUs busy"
                                (condorit--number (plist-get pool :gpus-used))
                                (condorit--number (plist-get pool :gpus-total)))
                      "")
                    (condorit--number (plist-get pool :machines))
                    (condorit--number (plist-get pool :jobs))
                    (condorit--refresh-state)))
    (condorit--insert-options)
    (condorit--insert-error)
    (insert "\n")
    (magit-insert-section (condorit-machines)
      (magit-insert-heading
        (if (= (length machines) (length all))
            (format "Machines (%d)" (length all))
          (format "Machines (%d of %d)" (length machines) (length all))))
      (if (not grouping)
          (progn (insert (condorit--header-row "  " condorit-pool--machine-columns))
                 (condorit-pool--insert-machines machines now "  " widths))
        (insert (condorit--header-row "    " condorit-pool--machine-columns))
        (pcase-dolist (`(,key . ,members)
                       (sort (seq-group-by (lambda (machine)
                                             (condorit-pool--group-key (nth 1 machine) grouping))
                                           machines)
                             (lambda (a b) (string< (car a) (car b)))))
          (magit-insert-section (condorit-machine-group key)
            (magit-insert-heading (format "  %s (%d)" key (length members)))
            (condorit-pool--insert-machines members now "    " widths)))))))

(defun condorit-pool--slot-name-at-point ()
  "Return the name of the slot at point; on a machine, its whole-machine slot."
  (let ((section (magit-current-section)))
    (pcase (and section (oref section type))
      ('condorit-slot (oref section value))
      ('condorit-machine
       (let ((slots (seq-filter (lambda (slot) (equal (plist-get slot :Machine)
                                                      (oref section value)))
                                (plist-get condorit--data :slots))))
         (or (plist-get (condorit-classad-machine-ad slots) :Name)
             (user-error (substitute-command-keys
                          "No data for machine %s; refresh with \\[condorit-refresh]")
                         (oref section value)))))
      (_ (user-error "No machine or slot at point")))))

(defun condorit-pool-visit (name)
  "Show the full ClassAd of slot NAME, as condor_status -l does.
Interactively, the slot at point; on a machine, its partitionable
or first static slot, which describes the whole machine."
  (interactive (list (condorit-pool--slot-name-at-point)))
  (condorit--show-ad (condorit--buffer-name "condorit-classad" name)
                    (or (condorit--fetch-ad "condor_status" (list "-json" name))
                        (user-error "Slot %s is no longer in the pool" name))))

(defvar-keymap condorit-pool-mode-map
  :doc "Keys of the pool view."
  :parent condorit-mode-map
  "RET" #'condorit-pool-visit)

(define-derived-mode condorit-pool-mode condorit-mode "Condorit-Pool"
  "Major mode for the HTCondor pool view.

\\{condorit-pool-mode-map}"
  (setq-local condorit--fetch-function #'condorit-pool--commands
              condorit--insert-function #'condorit-pool--insert))

;;;###autoload
(defun condorit-pool ()
  "Show the machines and slots of the HTCondor pool.
The view opens with every machine folded, whatever was unfolded the
last time it was shown."
  (interactive)
  (let ((buffer (condorit--view-buffer (condorit--buffer-name "condorit-pool")
                                      #'condorit-pool-mode)))
    (with-current-buffer buffer
      ;; magit-section remembers folded sections here across redraws.
      (setq magit-section-visibility-cache nil)
      (unless condorit--user
        (setq condorit--user (condorit--detect-user)))
      (condorit-refresh))
    (pop-to-buffer buffer)))

(provide 'condorit-pool)
;;; condorit-pool.el ends here
