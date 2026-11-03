;;; condorit-dispatch-test.el --- Tests for the dispatch menu  -*- lexical-binding: t; -*-

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

(ert-deftest condorit-dispatch-every-key-is-a-command ()
  (map-keymap (lambda (_key command)
                (when (and (symbolp command) command
                           (string-prefix-p "condorit" (symbol-name command)))
                  (should (commandp command))))
              condorit-status-mode-map)
  (should (commandp 'condorit-dispatch)))

(defun condorit-dispatch-test--key (mode key)
  "The command KEY runs in a buffer in MODE."
  (with-temp-buffer
    (funcall mode)
    (keymap-lookup (current-local-map) key)))

(ert-deftest condorit-dispatch-job-keys-only-in-job-views ()
  (dolist (key '("RET" "l"))
    (should-not (memq (condorit-dispatch-test--key 'condorit-pool-mode key) '(condorit-visit condorit-logs)))
    (should-not (condorit-dispatch-test--key 'condorit-users-mode key))
    (should (condorit-dispatch-test--key 'condorit-history-mode key))
    (should (condorit-dispatch-test--key 'condorit-status-mode key)))
  ;; Actions on queued jobs make no sense for finished ones.
  (dolist (key '("a" "k" "H" "R" "e"))
    (should-not (condorit-dispatch-test--key 'condorit-pool-mode key))
    (should-not (condorit-dispatch-test--key 'condorit-users-mode key))
    (should-not (condorit-dispatch-test--key 'condorit-history-mode key))
    (should (condorit-dispatch-test--key 'condorit-status-mode key)))
  (dolist (mode '(condorit-pool-mode condorit-users-mode
                  condorit-history-mode condorit-status-mode))
    (dolist (key '("g" "?" "$" "s" "t" "P" "U"))
      (should (condorit-dispatch-test--key mode key))))
  ;; RET in the pool view shows the ClassAd of a machine or slot.
  (should (eq (condorit-dispatch-test--key 'condorit-pool-mode "RET") 'condorit-pool-visit)))

(ert-deftest condorit-dispatch-job-group-only-in-job-views ()
  (let ((status (condorit-test-dispatch-text 'condorit-status-mode))
        (history (condorit-test-dispatch-text 'condorit-history-mode))
        (pool (condorit-test-dispatch-text 'condorit-pool-mode)))
    (should (string-match-p "Remove (condor_rm)" status))
    (should (string-match-p "Show full ClassAd" history))
    (should-not (string-match-p "Remove (condor_rm)" history))
    (should-not (string-match-p "Job(s) at point" pool))
    (should (string-match-p " P Pool$" pool))
    (should (string-match-p "Show full ClassAd (condor_status -l)" pool))
    (should-not (string-match-p "condor_status -l" status))))

(ert-deftest condorit-dispatch-options-per-view ()
  (should (string-match-p "All users' jobs" (condorit-test-dispatch-text 'condorit-status-mode)))
  ;; The history view's options come from its own menu: no "g" here,
  ;; which would apply none and drop them.
  (let ((history (condorit-test-dispatch-text 'condorit-history-mode)))
    (should (string-match-p "Sort jobs" history))
    (should-not (string-match-p "Refresh with these options" history)))
  ;; Each view previews its own command.
  (should (string-match-p "→ condor_q$" (condorit-test-dispatch-text 'condorit-status-mode)))
  (should (string-match-p "→ condor_userprio$" (condorit-test-dispatch-text 'condorit-users-mode)))
  (should (string-match-p "Include idle users" (condorit-test-dispatch-text 'condorit-users-mode)))
  (should (string-match-p "Only machines with free GPUs"
                          (condorit-test-dispatch-text 'condorit-pool-mode)))
  (should-not (string-match-p "Only machines"
                              (condorit-test-dispatch-text 'condorit-status-mode))))

(ert-deftest condorit-dispatch-pool-preview-shows-only-condor-options ()
  (let ((text (with-temp-buffer
                (condorit-pool-mode)
                (setq condorit-options '("--free-gpus" "-constraint=TotalGPUs > 0"))
                (transient-setup 'condorit-dispatch)
                (unwind-protect
                    (with-current-buffer " *transient*" (buffer-string))
                  (transient-quit-all)))))
    ;; condor_status gets only its own options; condorit's filters follow.
    (should (string-match-p
             "→ condor_status -constraint 'TotalGPUs > 0'   then in condorit: --free-gpus$" text))))

(ert-deftest condorit-dispatch-set-options-refreshes ()
  (let (fetched-options)
    (cl-letf (((symbol-function 'condorit-run-all)
               (lambda (specs &rest _)
                 (setq fetched-options (cdddr (nth 2 (assq 'jobs specs)))))))
      (condorit-test-with-status-buffer ()
        (condorit-set-options '("-allusers"))
        (should (equal condorit-options '("-allusers")))
        (should (equal fetched-options '("-allusers")))))))

(defun condorit-dispatch-test--press (mode keys)
  "Open the dispatch menu in MODE, press KEYS (e.g. \"- a\"), return (VALUE TEXT)."
  (with-temp-buffer
    (funcall mode)
    (transient-setup 'condorit-dispatch)
    (unwind-protect
        (progn
          (dolist (key keys)
            (let ((this-command (lookup-key transient--transient-map (kbd key))))
              (call-interactively this-command)
              (transient--redisplay)))
          (list (transient-get-value)
                (with-current-buffer " *transient*" (buffer-string))))
      (transient-quit-all))))

(ert-deftest condorit-dispatch-options-toggle-and-preview ()
  ;; Equal keys in other views (-a, -c) must not catch the key press.
  (pcase-let ((`(,value ,text) (condorit-dispatch-test--press 'condorit-status-mode '("- a"))))
    (should (equal value '("-allusers")))
    (should (string-match-p "→ condor_q -allusers$" text)))
  (pcase-let ((`(,value ,text) (condorit-dispatch-test--press 'condorit-users-mode '("- a"))))
    (should (equal value '("-allusers")))
    (should (string-match-p "→ condor_userprio -allusers$" text)))
  ;; The pool view's own filters show after the command.
  (pcase-let ((`(,value ,text) (condorit-dispatch-test--press 'condorit-pool-mode '("- G"))))
    (should (equal value '("--free-gpus")))
    (should (string-match-p "→ condor_status   then in condorit: --free-gpus$" text))))

(ert-deftest condorit-dispatch-constraint-reader-runs-in-the-view ()
  ;; The reader builds its examples from the row at point, so it must
  ;; run in the view's buffer when reached by real key presses.
  (condorit-test-with-status-buffer ()
    (let (seen)
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) (setq seen (current-buffer)) "")))
          (unwind-protect (execute-kbd-macro (kbd "? - c"))
            (when transient--prefix (transient-quit-all)))))
      (should (eq seen (current-buffer))))))

;;; condorit-dispatch-test.el ends here
