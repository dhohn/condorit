;;; condorit-submit.el --- Submit jobs from condorit  -*- lexical-binding: t; -*-

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
;; A transient menu for condor_submit, usable from any buffer: it
;; offers the submit file of the current buffer or Dired line.

;;; Code:

(require 'transient)
(require 'condorit-process)
(require 'condorit-base)

(declare-function dired-get-filename "dired")

(defun condorit-submit--default-file ()
  "Return the submit file of the current buffer or Dired line, or nil."
  (let ((file (or (and (derived-mode-p 'dired-mode) (dired-get-filename nil t))
                  buffer-file-name)))
    (and file (string-match-p "\\.\\(sub\\|submit\\|jdl\\|condor\\)\\'" file) file)))

(defun condorit-submit--args (args file)
  "Return condor_submit words for transient ARGS and submit FILE."
  (append (mapcan (lambda (arg)
                    (if (equal arg "-dry-run") (list "-dry-run" "-") (list arg)))
                  (condorit-expand-args args))
          (list file)))

(defun condorit-submit--run (args file)
  "Submit FILE with transient ARGS from FILE's directory."
  (let* ((default-directory (file-name-directory (expand-file-name file)))
         (words (condorit-submit--args args (file-name-nondirectory file))))
    (if (member "-dry-run" args)
        (condorit-run "condor_submit" words
                     (lambda (exit stdout stderr)
                       (if (eql exit 0)
                           (condorit--show-output (condorit--buffer-name "condorit-submit") "condor_submit" words stdout)
                         (message "%s" (condorit-error-summary "condor_submit" exit stdout stderr)))))
      (condorit--run-action "condor_submit" words))))

(transient-define-suffix condorit-submit-run (args)
  "Submit with the chosen options."
  (interactive (list (transient-args 'condorit-submit)))
  (condorit-submit--run args (or (transient-scope 'condorit-submit)
                                (read-file-name "Submit file: " nil nil t))))

;;;###autoload (autoload 'condorit-submit "condorit-submit" nil t)
(transient-define-prefix condorit-submit (file)
  "Submit jobs (condor_submit)."
  :man-page "condor_submit"
  ["Submit"
   ("-n" "Dry run: print the job ClassAd instead of submitting" "-dry-run")
   ("-b" "Batch name, shown by condor_q -batch" "-batch-name=")
   (:info (lambda ()
            (condorit--preview
             "condor_submit"
             (condorit-submit--args (transient-get-value)
                                   (let ((file (transient-scope 'condorit-submit)))
                                     (if file (file-name-nondirectory file) "FILE"))))))
   ("s" "Submit" condorit-submit-run)]
  (interactive (list (condorit-submit--default-file)))
  (transient-setup 'condorit-submit nil nil :scope file))

(provide 'condorit-submit)
;;; condorit-submit.el ends here
