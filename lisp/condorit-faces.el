;;; condorit-faces.el --- Faces and column layout for condorit  -*- lexical-binding: t; -*-

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
;; The faces condorit buffers use, the job-state legend, and helpers that
;; format labels, numbers, rows and their column headers.  Rows and
;; headers come from one column table, so the two cannot drift apart.
;; Text is propertized with `font-lock-face', because font-lock strips
;; plain `face' properties in these buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit-section)
(require 'condorit-classad)

;;;; Faces

(defgroup condorit-faces nil
  "Faces used by condorit."
  :group 'condorit
  :group 'faces)

(defface condorit-state-running '((t :inherit success))
  "Face for running jobs." :group 'condorit-faces)
(defface condorit-state-idle '((t :inherit font-lock-constant-face))
  "Face for idle jobs." :group 'condorit-faces)
(defface condorit-state-held '((t :inherit error))
  "Face for held jobs." :group 'condorit-faces)
(defface condorit-state-transferring '((t :inherit warning))
  "Face for jobs transferring output." :group 'condorit-faces)
(defface condorit-state-suspended '((t :inherit warning))
  "Face for suspended jobs." :group 'condorit-faces)
(defface condorit-state-completed '((t :inherit shadow))
  "Face for completed jobs." :group 'condorit-faces)
(defface condorit-state-removed '((t :inherit shadow))
  "Face for removed jobs." :group 'condorit-faces)
(defface condorit-failed '((t :inherit error))
  "Face for failed exit codes and failure counts." :group 'condorit-faces)
(defface condorit-hold-reason '((t :inherit warning))
  "Face for hold reasons." :group 'condorit-faces)
(defface condorit-id '((t :inherit font-lock-type-face))
  "Face for job and cluster ids." :group 'condorit-faces)
(defface condorit-number '((t :inherit font-lock-number-face))
  "Face for durations, counts and priorities." :group 'condorit-faces)
(defface condorit-column-header '((t :inherit shadow :underline t))
  "Face for column header rows." :group 'condorit-faces)
(defface condorit-header-label '((t :inherit magit-section-heading))
  "Face for labels like Schedd: and Pool: at the top of a buffer." :group 'condorit-faces)
(defface condorit-slot-claimed '((t :inherit success))
  "Face for claimed slots." :group 'condorit-faces)
(defface condorit-slot-unclaimed '((t :inherit shadow))
  "Face for unclaimed slots." :group 'condorit-faces)

(defun condorit-state-face (state)
  "Return the face for job STATE, e.g. `condorit-state-held' for `held'."
  (let ((face (and state (intern (format "condorit-state-%s" state)))))
    (if (facep face) face 'default)))

(defun condorit--propertize (string face)
  "Return STRING with FACE as its `font-lock-face'."
  (propertize string 'font-lock-face face))

(defun condorit--label (label)
  "Return LABEL padded to the header label width, in the label face."
  (concat (condorit--propertize label 'condorit-header-label)
          (make-string (max 0 (- 8 (length label))) ?\s)))

(defun condorit--number (n)
  "Return the number N as a string in the `condorit-number' face."
  (condorit--propertize (number-to-string n) 'condorit-number))

;;;; Legend

(defconst condorit--legend-order
  '(running idle held transferring suspended completed removed)
  "Job states in the order the legend and state counts list them.")

(defun condorit-state-legend (&optional property)
  "Return \"R running  I idle ...\", each letter in its state face.
PROPERTY is the text property used for the faces, default `font-lock-face'."
  (mapconcat (lambda (state)
               (concat (propertize (condorit-classad-state-letter state)
                                   (or property 'font-lock-face) (condorit-state-face state))
                       " " (downcase (condorit-classad-state-name state))))
             condorit--legend-order "  "))

;;;; Columns

(defun condorit--cell (string width &optional align)
  "Pad or truncate STRING to WIDTH; nil WIDTH leaves it alone.
ALIGN `right' pads on the left instead and never truncates, for numbers."
  (cond ((null width) string)
        ((eq align 'right) (string-pad string width nil t))
        (t (truncate-string-to-width string width nil ?\s "…"))))

(defun condorit--row (indent columns cells)
  "Format CELLS as a row laid out by COLUMNS, after INDENT.
COLUMNS is a list of (TITLE WIDTH [ALIGN]); the last WIDTH may be nil.
ALIGN `right' right-aligns the column, e.g. for numbers."
  (concat indent
          (string-trim-right
           (string-join (cl-mapcar (lambda (cell column)
                                     (condorit--cell cell (nth 1 column) (nth 2 column)))
                                   cells columns)
                        " ")
           " +")))

(defun condorit--header-row (indent columns)
  "Return the header row for COLUMNS after INDENT, ending in a newline."
  (concat (condorit--row indent columns
                        (mapcar (lambda (column)
                                  (condorit--propertize (car column) 'condorit-column-header))
                                columns))
          "\n"))

(defconst condorit--job-columns
  '(("ID" 14) ("BATCH" 20) ("STATE" 22) ("TIME / DETAIL" nil))
  "Columns shared by cluster rows and job rows.
A job row nested under its cluster indents its id inside the ID
column, so all other columns line up with the cluster rows.")

(provide 'condorit-faces)
;;; condorit-faces.el ends here
