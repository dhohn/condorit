;;; condorit-remote-test.el --- Tests for remote (TRAMP) use  -*- lexical-binding: t; -*-

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

(defmacro condorit-test-with-direct-async (&rest body)
  "Run BODY with TRAMP direct async processes enabled for the mock method."
  (declare (indent 0))
  `(let ((criteria '(:application tramp :protocol "mock")))
     (connection-local-set-profile-variables
      'condorit-test-direct '((tramp-direct-async-process . t)))
     (connection-local-set-profiles criteria 'condorit-test-direct)
     (unwind-protect (progn ,@body)
       (setq connection-local-criteria-alist
             (seq-remove (lambda (entry) (equal (car entry) criteria))
                         connection-local-criteria-alist)))))


(ert-deftest condorit-remote-run-all-several-commands ()
  ;; Starting several async processes at once through TRAMP fails
  ;; unless direct async is enabled; condorit must still get all results.
  (condorit-test-with-fake-condor '(("condor_q" :out "[]") ("condor_status" :out "[]")
                                   ("condor_history" :out "") ("condor_userprio" :out ""))
    (condorit-test-with-remote
      (let (results)
        (condorit-run-all '((jobs "condor_q" ("-json")) (finished "condor_history" ("-json"))
                           (slots "condor_status" ("-json")) (userprio "condor_userprio" ("-long")))
                         (lambda (r) (setq results r)))
        (condorit-test-wait (lambda () results) 30)
        (should (equal (sort (mapcar #'car results) #'string<)
                       '(finished jobs slots userprio)))
        (should (cl-every (lambda (r) (eql (nth 2 r) 0)) results))))))

(ert-deftest condorit-remote-batch-split ()
  (let ((m "@M@"))
    (should (equal (condorit--batch-split
                    (concat "login noise\n"
                            "\n@M@ 0 0\n[1]\n\n@M@\n"
                            "\n@M@ 1 2\nno newline at end\n@M@\nboom\n\n@M@ end\n")
                    m)
                   '((0 0 "[1]\n" "") (1 2 "no newline at end" "boom\n"))))))

(defun condorit-remote-test--batch ()
  "Run a failing and a succeeding command remotely; return their results."
  (let (results)
    (condorit-run-all '((jobs "condor_q" ("-json")) (slots "condor_status" ("-json" "x y")))
                     (lambda (r) (setq results r)))
    (condorit-test-wait (lambda () results) 30)
    results))

(ert-deftest condorit-remote-batch-results-and-log ()
  ;; One shell runs every command; each still gets its own exit code,
  ;; output and line in the command log.
  (dolist (direct '(nil t))
    (condorit-test-with-fake-condor '(("condor_q" :err "schedd down\n" :exit 1)
                                     ("condor_status" :out "[]\n"))
      (condorit-test-with-remote
        (when (get-buffer condorit-log-buffer-name)
          (kill-buffer condorit-log-buffer-name))
        (let ((results (if direct
                           (condorit-test-with-direct-async (condorit-remote-test--batch))
                         (condorit-remote-test--batch))))
          (should (equal results '((jobs "condor_q" 1 "" "schedd down\n")
                                   (slots "condor_status" 0 "[]\n" ""))))
          (should (member '("condor_status" "-json" "x y") (condorit-test-argv)))
          (with-current-buffer condorit-log-buffer-name
            (should (string-match-p "\\$ condor_q -json   \\[exit 1\\]" (buffer-string)))
            (should (string-match-p "\\$ condor_status -json 'x y'   \\[exit 0\\]" (buffer-string)))
            (should-not (string-match-p "\\$ sh " (buffer-string)))))))))

(ert-deftest condorit-remote-status-refresh ()
  (condorit-test-with-fake-condor (condorit-test-fixture-responses)
    (condorit-test-with-remote
      (with-temp-buffer
        (setq default-directory (concat "/mock::" temporary-file-directory))
        (condorit-status-mode)
        (setq condorit--host "submit.example.org" condorit--user "alice")
        (condorit-refresh)
        (condorit-test-wait (lambda () (not condorit--pending)) 30)
        (should-not condorit--error)
        (should (string-match-p "^Held (2)" (buffer-string)))))))

(ert-deftest condorit-remote-buffer-names ()
  (let ((default-directory temporary-file-directory))
    (should (equal (condorit--buffer-name "condorit-history") "*condorit-history*"))
    (should (equal (condorit--buffer-name "condorit-classad" "1.0") "*condorit-classad: 1.0*")))
  (let ((default-directory "/ssh:login1:/home/a/"))
    (should (equal (condorit--buffer-name "condorit-history") "*condorit-history@login1*"))
    (should (equal (condorit--buffer-name "condorit-classad" "1.0") "*condorit-classad@login1: 1.0*"))))

(ert-deftest condorit-remote-schedd-fallback-uses-host ()
  (condorit-test-with-fake-condor '(("condor_config_val" :err "Not defined\n" :exit 1))
    (condorit-test-with-remote
      (should (equal (condorit--detect-schedd)
                     (file-remote-p default-directory 'host))))))

(ert-deftest condorit-remote-prefix-arg-asks-for-host ()
  (let (asked opened)
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) (setq asked t) "/ssh:login1:/home/a/"))
              ((symbol-function 'condorit-status) (lambda (&optional dir) (setq opened dir))))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'condorit))
      (should asked)
      (should (equal opened "/ssh:login1:/home/a/"))
      (setq asked nil opened 'unset)
      (let ((current-prefix-arg nil))
        (call-interactively #'condorit))
      (should-not asked)
      (should (null opened)))))

(ert-deftest condorit-remote-default-directory-option ()
  (let (opened)
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) "/ssh:login2:/home/a/"))
              ((symbol-function 'condorit-status) (lambda (&optional dir) (setq opened dir))))
      (let ((condorit-default-directory "/ssh:login1:"))
        (let ((current-prefix-arg nil))
          (call-interactively #'condorit))
        (should (equal opened "/ssh:login1:"))
        ;; The prefix argument still asks, and wins.
        (let ((current-prefix-arg '(4)))
          (call-interactively #'condorit))
        (should (equal opened "/ssh:login2:/home/a/"))))))

(ert-deftest condorit-remote-run-avoids-tramp-stderr-buffers ()
  ;; Without direct async, TRAMP implements a :stderr buffer with an
  ;; extra remote shell that stalls now and then for ~5s.  condorit must
  ;; capture stderr separately without asking TRAMP for one.
  (condorit-test-with-fake-condor '(("condor_q" :out "[]\n" :err "warn\n" :exit 2))
    (condorit-test-with-remote
      (let ((stderr-args 0))
        (cl-letf* ((orig (symbol-function 'make-process))
                   ((symbol-function 'make-process)
                    (lambda (&rest args)
                      (when (plist-get args :stderr) (cl-incf stderr-args))
                      (apply orig args))))
          (dotimes (_ 5)
            (let (result)
              (condorit-run "condor_q" '("-json" "a b") (lambda (&rest r) (setq result r)))
              (condorit-test-wait (lambda () result) 15)
              (should (equal result '(2 "[]\n" "warn\n"))))))
        (should (= stderr-args 0))
        (should (equal (car (last (condorit-test-argv))) '("condor_q" "-json" "a b")))))))

(ert-deftest condorit-remote-direct-async-keeps-stdout-clean ()
  ;; With direct async, output of the login itself (here: the mock
  ;; shell's job-control warning; with ssh: banners, host-key notes)
  ;; arrives on stderr and must not end up in stdout.
  (condorit-test-with-fake-condor '(("condor_q" :out "[]\n" :err "warn\n"))
    (condorit-test-with-remote
      (condorit-test-with-direct-async
        (let (result)
          (condorit-run "condor_q" '("-json") (lambda (&rest r) (setq result r)))
          (condorit-test-wait (lambda () result) 15)
          (should (equal (nth 1 result) "[]\n"))
          (should (string-match-p "warn" (nth 2 result))))))))

;;; condorit-remote-test.el ends here
