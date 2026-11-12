;;; -*- coding: utf-8; lexical-binding: t -*-
;;; shots.el -- screenshot machinery for fancy scenes
;;;
;;; A scene file is plain elisp that arranges windows/buffers and ends by
;;; calling (fancy:shoot FILE). Run via bin/shot.

(defvar fancy:shot-default-method 'frame
  "Default capture method: `frame' (x-export-frames) or `window' (xwd).")

(defun fancy:shoot (file &optional method)
  "Save a screenshot to FILE.
METHOD `frame' exports the selected frame (no child frames / posframes).
METHOD `window' captures the frame's X window via xwd (includes posframes)."
  (redisplay t)
  (sit-for 0.3)
  (pcase (or method fancy:shot-default-method)
    ('frame
     (let ((data (x-export-frames nil 'png)))
       (with-temp-file file
         (set-buffer-multibyte nil)
         (insert data))))
    ('window
     (let ((wid (frame-parameter nil 'outer-window-id))
           (xwd (or (getenv "XWD") "xwd"))
           (magick (or (getenv "MAGICK") "magick"))
           (xwd-file (concat file ".xwd")))
       (call-process xwd nil nil nil "-silent" "-nobdrs"
                     "-id" wid "-out" xwd-file)
       (call-process magick nil nil nil xwd-file file)
       (delete-file xwd-file))))
  (message "shot -> %s" file))

(defun fancy:frame-center ()
  "Roughly center the frame on a 1920x1200 screen."
  (let* ((w (frame-pixel-width))
         (h (frame-pixel-height)))
    (set-frame-position (selected-frame)
                        (max 0 (/ (- 1920 w) 2))
                        (max 0 (/ (- 1200 h) 2)))))

;;; always leave a log next to the png so failures are diagnosable
(defun fancy:dump-state (file)
  "Write *Messages*/*Warnings*/backtrace-ish state to FILE."
  (with-temp-file file
    (dolist (name '("*Messages*" "*Warnings*" "*Compile-Log*" "*Backtrace*"))
      (when-let* ((b (get-buffer name)))
        (insert (format "\n===== %s =====\n" name)
                (with-current-buffer b (buffer-string)))))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (when-let* ((out (getenv "FANCY_OUT")))
              (fancy:dump-state (concat out ".log")))))

;; if the emacs frame dies for whatever reason bin/shot notices a missing png;
;; also make sure scene errors land in the log instead of hanging the debugger
(setq debug-on-error nil)
