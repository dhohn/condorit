;;; condorit.el --- Magit-like HTCondor interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Hohn

;; Author: David Hohn <hohn.david@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.8.0") (magit-section "4.0.0"))
;; Keywords: tools, processes
;; URL: https://github.com/dhohn/condorit

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
;; condorit shows your HTCondor jobs and pool in a Magit-style buffer.
;; Every action is a transient menu that shows the exact condor
;; command it runs, so you learn the CLI while using it.
;; Start with M-x condorit.

;;; Code:

(require 'condorit-process)
(require 'condorit-classad)
(require 'condorit-faces)
(require 'condorit-base)
(require 'condorit-jobs)
(require 'condorit-constraint)
(require 'condorit-actions)
(require 'condorit-submit)
(require 'condorit-status)
(require 'condorit-pool)
(require 'condorit-users)
(require 'condorit-history)
(require 'condorit-dispatch)

(defcustom condorit-default-directory nil
  "Directory whose host `condorit' uses when called without a prefix.
A TRAMP directory such as \"/ssh:login-node:\" makes `condorit' work
with the schedd there from any buffer.  nil means the host of
`default-directory'."
  :type '(choice (const :tag "Host of the current buffer" nil)
                 (directory :tag "Directory"))
  :group 'condorit)

;;;###autoload
(defun condorit (&optional directory)
  "Open the HTCondor status buffer.
Use the host of DIRECTORY, which defaults to
`condorit-default-directory' or else `default-directory'.  With a
prefix argument, ask for DIRECTORY, e.g. /ssh:login-node: to work
with the schedd there through TRAMP."
  (interactive
   (list (and current-prefix-arg
              (read-directory-name "Condor host (directory): " "/ssh:"))))
  (condorit-status (or directory condorit-default-directory)))

(provide 'condorit)
;;; condorit.el ends here
