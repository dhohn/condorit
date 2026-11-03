;;; record.el --- Record condorit's views against the live pool  -*- lexical-binding: t; -*-

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

;; Run by test/live/record:  emacs --batch -l record.el DIR
;;
;; Opens every view with the condor_* commands of test/live/bin first on
;; `exec-path', then refreshes each once more with CONDORIT_RECORD set, so
;; that refresh's commands save what they printed in DIR/VIEW/.  The
;; version tests replay them.

;;; Code:

(require 'condorit)

(defun condorit-record--wait ()
  "Wait for the refresh of the current buffer, or fail after 60 seconds."
  (let ((deadline (+ (float-time) 60)))
    (while (and condorit--pending (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (when condorit--pending
      (error "%s: refresh did not finish" major-mode))))

(let ((dir (pop command-line-args-left))
      (exec-path (cons (expand-file-name "bin" (file-name-directory load-file-name))
                       exec-path)))
  (dolist (view `((status . condorit-status)
                  (history . ,(lambda ()
                                (condorit-history--open '("--mine") (condorit--detect-user))))
                  (pool . condorit-pool)
                  (users . condorit-users)))
    (funcall (cdr view))
    (with-current-buffer (window-buffer (selected-window))
      (condorit-record--wait)
      (let ((process-environment
             (cons (concat "CONDORIT_RECORD=" (expand-file-name (symbol-name (car view)) dir))
                   process-environment)))
        (condorit-refresh))
      (condorit-record--wait)
      (when condorit--error
        (error "%s: %s" (car view) condorit--error))
      (message "%s: recorded" (car view)))))

;;; record.el ends here
