;;; condorit-versions-test.el --- Replay real HTCondor versions  -*- lexical-binding: t; -*-

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

;; test/fixtures/versions/VERSION/VIEW/ holds what that HTCondor version
;; printed for the commands of a refresh of VIEW, recorded by
;; test/live/record after test/live/scenario.  These tests replay every
;; recorded version and check what the scenario guarantees.

;;; Code:

(require 'condorit-test-helper)
(require 'condorit)

(defconst condorit-versions-test-dir
  (expand-file-name "versions" condorit-test-fixtures-dir))

(defun condorit-versions-test--versions ()
  "Return the recorded versions."
  (and (file-directory-p condorit-versions-test-dir)
       (directory-files condorit-versions-test-dir nil "\\`[0-9]")))

(defun condorit-versions-test--responses (dir)
  "Return the fake condor responses recorded in DIR."
  (let (responses)
    (dolist (file (directory-files dir t "\\.\\(out\\|err\\|exit\\)\\'"))
      (let* ((program (file-name-base file))
             (key (intern (concat ":" (file-name-extension file))))
             (text (with-temp-buffer (insert-file-contents file) (buffer-string)))
             (response (or (assoc program responses)
                           (car (push (list program) responses)))))
        (setcdr response (plist-put (cdr response) key
                                    (if (eq key :exit) (string-to-number text) text)))))
    responses))

(defmacro condorit-versions-test--each (view mode &rest body)
  "Run BODY for every recorded version, in a MODE buffer refreshed from VIEW."
  (declare (indent 2))
  `(dolist (version (condorit-versions-test--versions))
     (ert-info (version :prefix "HTCondor ")
       (condorit-test-with-fake-condor
           (condorit-versions-test--responses
            (expand-file-name ,view (expand-file-name version condorit-versions-test-dir)))
         (with-temp-buffer
           (,mode)
           (setq condorit--host "submit.example.org"
                 condorit--user "alice")
           (condorit-refresh)
           (condorit-test-wait (lambda () (not condorit--pending)))
           (should-not condorit--error)
           ,@body)))))

(defun condorit-versions-test--batches (jobs)
  "Return ((BATCH . STATE) ...) of JOBS, sorted."
  (sort (mapcar (lambda (job) (cons (plist-get job :JobBatchName) (condorit-classad-state job)))
                jobs)
        (lambda (a b) (string< (format "%s" a) (format "%s" b)))))

(ert-deftest condorit-versions-recorded ()
  (should (condorit-versions-test--versions)))

(ert-deftest condorit-versions-status ()
  (condorit-versions-test--each "status" condorit-status-mode
    (let ((jobs (plist-get condorit--data :jobs))
          (finished (plist-get condorit--data :finished))
          (text (buffer-string)))
      (should (equal (condorit-versions-test--batches jobs)
                     '(("held" . held) ("policy" . held) ("running" . running)
                       ("running" . running) ("toobig" . idle))))
      (should (equal (condorit-versions-test--batches finished)
                     '(("done" . completed) ("failed" . completed) ("removed" . removed))))
      (should (equal (plist-get (seq-find (lambda (j) (equal (plist-get j :JobBatchName) "failed"))
                                          finished)
                                :ExitCode)
                     3))
      (should (string-match-p "^Held (2)" text))
      (should (string-match-p "Held by on_exit_hold" text))
      (should (string-match-p "^My jobs (5)" text))
      (should (string-match-p "toobig +I 1 +✗ 1 too big for any slot" text))
      (should (string-match-p "^Recently finished (last 24h) (3)" text))
      (should (string-match-p "failed +C 1 +✗ 1 failed" text))
      (should (string-match-p "2/4 CPUs busy" text))
      (should (condorit-status--user-priority (plist-get condorit--data :userprio) "alice")))))

(ert-deftest condorit-versions-history ()
  (condorit-versions-test--each "history" condorit-history-mode
    (should (equal (condorit-versions-test--batches (plist-get condorit--data :jobs))
                   '(("done" . completed) ("failed" . completed) ("removed" . removed))))
    (should (string-match-p "^Finished jobs (3)" (buffer-string)))))

(ert-deftest condorit-versions-pool ()
  (condorit-versions-test--each "pool" condorit-pool-mode
    (should (equal (condorit-classad-pool-summary (plist-get condorit--data :slots))
                   '(:machines 1 :cpus-total 4 :cpus-used 2 :jobs 2 :gpus-total 0 :gpus-used 0)))
    (should (string-match-p "submit" (buffer-string)))))

(ert-deftest condorit-versions-users ()
  (condorit-versions-test--each "users" condorit-users-mode
    (should (string-match-p "alice@submit.example.org" (buffer-string)))
    (should (string-match-p "submit.example.org" (buffer-string)))))

;;; condorit-versions-test.el ends here
