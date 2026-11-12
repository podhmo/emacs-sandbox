;;; probe.el --- dump startup state of a GUI emacs for bin/check -*- lexical-binding: t -*-
;;
;; Loaded from the *end of early-init.el* of a throwaway copy of rebirth/
;; (see bin/check).  Loading this early means the fallback timer is armed even
;; if init.el errors and the debugger sits in a recursive edit -- timers still
;; fire there, so a report is always written.
;;
;; env:
;;   REBIRTH_CHECK_OUT   output directory (report.txt / messages.log)
;;   REBIRTH_CHECK_WAIT  seconds after emacs-startup-hook before dumping (default 3)
;;   REBIRTH_CHECK_LIMIT fallback seconds from early-init (default 60)

(defvar rebirth-check--out (getenv "REBIRTH_CHECK_OUT"))
(defvar rebirth-check--done nil)
(defvar rebirth-check--phase "early-init")
(defvar rebirth-check--t0 (current-time))

(defun rebirth-check--buffer (name)
  (if-let* ((b (get-buffer name)))
      (with-current-buffer b (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(defun rebirth-check--dump ()
  (unless rebirth-check--done
    (setq rebirth-check--done t)
    (redisplay t)
    (let* ((warnings (rebirth-check--buffer "*Warnings*"))
           (backtrace (rebirth-check--buffer "*Backtrace*"))
           (ok (and (eq rebirth-check--phase 'startup-done)
                    (not init-file-had-error)
                    (string-empty-p backtrace))))
      (with-temp-file (expand-file-name "messages.log" rebirth-check--out)
        (dolist (name '("*Messages*" "*Warnings*" "*Compile-Log*" "*Backtrace*"))
          (insert (format "\n===== %s =====\n%s" name (rebirth-check--buffer name)))))
      (with-temp-file (expand-file-name "report.txt" rebirth-check--out)
        (let ((print-length nil) (print-level nil))
          (dolist (kv
                   `((status . ,(if ok (quote OK) (quote NG)))
                     (phase . ,rebirth-check--phase)
                     (elapsed . ,(format "%.2fs" (float-time (time-since rebirth-check--t0))))
                     (emacs-version . ,emacs-version)
                     (window-system . ,window-system)
                     (user-emacs-directory . ,user-emacs-directory)
                     (init-file-had-error . ,init-file-had-error)
                     (has-warnings . ,(not (string-empty-p warnings)))
                     (has-backtrace . ,(not (string-empty-p backtrace)))
                     (my:use-after-init . ,(bound-and-true-p my:use-after-init))
                     (custom-enabled-themes . ,custom-enabled-themes)
                     (frame-font . ,(frame-parameter nil 'font))
                     (frame-size . ,(list (frame-width) (frame-height)
                                          (frame-pixel-width) (frame-pixel-height)))
                     (package-activated-list . ,(bound-and-true-p package-activated-list))
                     (treesit-available . ,(and (fboundp 'treesit-available-p) (treesit-available-p)))
                     (native-comp . ,(and (fboundp 'native-comp-available-p) (native-comp-available-p)))
                     (major-mode . ,major-mode)
                     (buffers . ,(mapcar #'buffer-name (buffer-list)))))
            (insert (format "%s: %S\n" (car kv) (cdr kv))))))
      ;; init.el opens the latest ~/memo/memo*.md (private notes).  Swap the
      ;; visible buffer before the screenshot so it never leaks into a shot.
      (when-let* ((file (getenv "REBIRTH_CHECK_FILE")))
        (delete-other-windows)
        (switch-to-buffer (find-file-noselect file))
        (dolist (b (buffer-list))
          (when (and (buffer-file-name b) (not (eq b (current-buffer))))
            (kill-buffer b)))
        (setq buffer-read-only t)
        (redisplay t)
        (sit-for 0.5))
      ;; touched last: bin/check waits for this file
      (with-temp-file (expand-file-name "ready" rebirth-check--out)
        (insert (number-to-string (emacs-pid)) "\n")))))

(when rebirth-check--out
  (make-directory rebirth-check--out t)
  ;; never let the user's daily server be taken over by the probe instance
  (setq server-name (format "rebirth-check-%d" (emacs-pid)))
  (add-hook 'after-init-hook (lambda () (setq rebirth-check--phase 'after-init)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq rebirth-check--phase 'startup-done)
              (run-at-time (string-to-number (or (getenv "REBIRTH_CHECK_WAIT") "3"))
                           nil #'rebirth-check--dump))
            90)
  (run-at-time (string-to-number (or (getenv "REBIRTH_CHECK_LIMIT") "60"))
               nil #'rebirth-check--dump))

;;; probe.el ends here
