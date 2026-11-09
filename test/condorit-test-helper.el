;;; condorit-test-helper.el --- Shared utilities for condorit tests  -*- lexical-binding: t; -*-

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
;; Fixture loading, a fake condor on `exec-path', and waiting for
;; asynchronous processes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defconst condorit-test-dir
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst condorit-test-fixtures-dir (expand-file-name "fixtures" condorit-test-dir))
(defconst condorit-test-bin-dir (expand-file-name "bin" condorit-test-dir))

(when (boundp 'native-comp-enable-subr-trampolines)
  ;; `cl-letf' on a primitive makes Emacs native-compile a trampoline,
  ;; which fails where the native compiler is broken (and is slow where
  ;; it works).  condorit is byte-compiled in the tests, so its calls go
  ;; through the symbol and see the mock anyway.
  (setq native-comp-enable-subr-trampolines nil))

(defconst condorit-test-now 1791970000
  "Fixed \"current time\" for tests; the fixtures are relative to it.")

(defvar condorit-test--arglog nil
  "File the fake condor appends its invocations to.")

(defun condorit-test-fixture (name)
  "Return the contents of fixture file NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name condorit-test-fixtures-dir))
    (buffer-string)))

(defun condorit-test-fixture-responses ()
  "Fake responses for the four commands of a status refresh."
  `(("condor_q" :out ,(condorit-test-fixture "condor_q.json"))
    ("condor_history" :out ,(condorit-test-fixture "condor_history.json"))
    ("condor_status" :out ,(condorit-test-fixture "condor_status.json"))
    ("condor_userprio" :out ,(condorit-test-fixture "condor_userprio.long"))))

(defun condorit-test--write (dir name content)
  (with-temp-file (expand-file-name name dir)
    (insert content)))

(defmacro condorit-test-with-fake-condor (responses &rest body)
  "Run BODY with the fake condor commands first on `exec-path'.
RESPONSES is a list of (PROGRAM :out STRING :err STRING :exit N); every
call of PROGRAM prints that output and exits with N (default 0).
Use `condorit-test-argv' in BODY to see what was run."
  (declare (indent 1))
  (let ((dir (make-symbol "dir")))
    `(let* ((,dir (make-temp-file "condorit-fake" t))
            (condorit-test--arglog (expand-file-name "argv.log" ,dir))
            (exec-path (cons condorit-test-bin-dir exec-path))
            (process-environment
             (append (list (concat "CONDORIT_TEST_RESPONSES=" ,dir)
                           (concat "CONDORIT_TEST_ARGLOG=" condorit-test--arglog))
                     process-environment)))
       (unwind-protect
           (progn
             (dolist (response ,responses)
               (let ((program (car response))
                     (spec (cdr response)))
                 (when (plist-get spec :out)
                   (condorit-test--write ,dir (concat program ".out") (plist-get spec :out)))
                 (when (plist-get spec :err)
                   (condorit-test--write ,dir (concat program ".err") (plist-get spec :err)))
                 (when (plist-get spec :exit)
                   (condorit-test--write ,dir (concat program ".exit")
                                        (number-to-string (plist-get spec :exit))))))
             ,@body)
         (delete-directory ,dir t)))))

(defun condorit-test-argv ()
  "Return the fake condor invocations so far, as lists of strings."
  (when (and condorit-test--arglog (file-exists-p condorit-test--arglog))
    (mapcar (lambda (line) (split-string line "\t"))
            (split-string (with-temp-buffer
                            (insert-file-contents condorit-test--arglog)
                            (buffer-string))
                          "\n" t))))

(defun condorit-test-wait (predicate &optional timeout)
  "Process events until PREDICATE returns non-nil, or fail after TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall predicate))))

(require 'condorit)

(defconst condorit-test-userprio-spec
  '((userprio "condor_userprio" nil condorit-classad-parse-userprio))
  "Specs naming the userprio parser, as the status view's commands do.")

(defun condorit-test-data ()
  "Status data built from the fixtures."
  (list :jobs (condorit-classad-parse (condorit-test-fixture "condor_q.json"))
        :finished (condorit-classad-parse (condorit-test-fixture "condor_history.json"))
        :slots (condorit-classad-parse (condorit-test-fixture "condor_status.json"))
        :userprio (condorit-classad-parse-long (condorit-test-fixture "condor_userprio.long"))
        :time condorit-test-now))

(defun condorit-test-status-buffer (&optional options data)
  "Return a status buffer rendered from DATA (default: fixtures), no processes."
  (let ((buffer (generate-new-buffer "*condorit-test*")))
    (with-current-buffer buffer
      (condorit-status-mode)
      (setq condorit--host "submit.example.org"
            condorit--user "alice"
            condorit-options options
            condorit--data (or data (condorit-test-data)))
      (condorit--render))
    buffer))

(defmacro condorit-test-with-status-buffer (args &rest body)
  "Run BODY in a status buffer made by `condorit-test-status-buffer', then kill it.
ARGS are that function's arguments, (OPTIONS DATA)."
  (declare (indent 1))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (condorit-test-status-buffer ,@args)))
       (unwind-protect (with-current-buffer ,buffer ,@body)
         (kill-buffer ,buffer)))))

(defmacro condorit-test-in-buffer (buffer-or-name &rest body)
  "Run BODY in BUFFER-OR-NAME, then kill that buffer, also when BODY fails."
  (declare (indent 1))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (get-buffer ,buffer-or-name)))
       (should ,buffer)
       (unwind-protect (with-current-buffer ,buffer ,@body)
         (kill-buffer ,buffer)))))

(defun condorit-test-dispatch-text (mode)
  "Return the text of the dispatch menu as it shows in a buffer in MODE."
  (with-temp-buffer
    (funcall mode)
    (transient-setup 'condorit-dispatch)
    (unwind-protect
        (with-current-buffer " *transient*" (buffer-string))
      (transient-quit-all))))

(defun condorit-test-find-section (type value)
  "Return the first section of TYPE with VALUE in the current buffer."
  (let (found)
    (cl-labels ((walk (section)
                  (when (and (not found)
                             (eq (oref section type) type)
                             (equal (oref section value) value))
                    (setq found section))
                  (mapc #'walk (oref section children))))
      (walk magit-root-section))
    found))

(defun condorit-test-folded-p (section)
  "Return non-nil if the body of SECTION is folded away on screen.
Checks the text itself, not only the section's `hidden' slot."
  (and (oref section hidden)
       (invisible-p (oref section content))))

(defun condorit-test-goto (type value)
  "Move point to the section of TYPE with VALUE."
  (let ((section (condorit-test-find-section type value)))
    (should section)
    (goto-char (oref section start))))

;;;; Remote (TRAMP) testing

(require 'tramp)

;; A TRAMP method that runs a local shell through the same code path as
;; /ssh: (tramp-sh), as TRAMP's own test suite does.
(add-to-list 'tramp-methods
             '("mock"
               (tramp-login-program "sh")
               (tramp-login-args (("-i")))
               (tramp-direct-async ("-c"))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c"))
               (tramp-connection-timeout 10)))

(defmacro condorit-test-with-remote (&rest body)
  "Run BODY with `default-directory' on the mock TRAMP host.
The fake condor commands come first on the remote PATH."
  (declare (indent 0))
  `(let ((tramp-remote-path (cons condorit-test-bin-dir tramp-remote-path))
         (tramp-verbose 0)
         (default-directory (concat "/mock::" temporary-file-directory)))
     (tramp-cleanup-all-connections)
     (unwind-protect (progn ,@body)
       (tramp-cleanup-all-connections))))

(provide 'condorit-test-helper)
;;; condorit-test-helper.el ends here
