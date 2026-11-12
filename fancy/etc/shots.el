;;; -*- coding: utf-8; lexical-binding: t -*-
;;; shots.el -- screenshot machinery for fancy scenes
;;;
;;; A scene file is plain elisp that arranges windows/buffers and ends by
;;; calling (fancy:shoot FILE). Run via bin/shot.

(defvar fancy:shot-default-method 'frame
  "Default capture method: `frame' (x-export-frames) or `window' (xwd).")

(defun fancy:shoot--ns (file)
  "macOS: capture the frame's on-screen rectangle with screencapture.
NS has no `x-export-frames', and `-l <window>' would drop child frames
(posframes are separate NSWindows), so grab the outer-edges rectangle --
same coordinate space (points, top-left origin) as `screencapture -R'.
Needs Screen Recording permission for the app that launched emacs."
  (x-focus-frame nil)                   ; activate + raise: nothing may occlude
  (redisplay t)
  (sit-for 0.5)
  (pcase-let ((`(,l ,top ,r ,b) (frame-edges nil 'outer-edges)))
    (call-process "screencapture" nil nil nil "-x" "-R"
                  (format "%d,%d,%d,%d" l top (- r l) (- b top))
                  file)))

(defun fancy:shoot (file &optional method)
  "Save a screenshot to FILE.
METHOD `frame' exports the selected frame (no child frames / posframes).
METHOD `window' captures the frame's X window via xwd (includes posframes).
On macOS (ns) both methods capture the frame rectangle, posframes included."
  (redisplay t)
  (sit-for 0.3)
  (pcase (if (eq window-system 'ns) 'ns (or method fancy:shot-default-method))
    ('ns (fancy:shoot--ns file))
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
  "Roughly center the frame on the screen (1920x1200 if unknown).
WSLg reports the whole virtual desktop, so it keeps the fixed size."
  (let* ((w (frame-pixel-width))
         (h (frame-pixel-height))
         (ns (eq window-system 'ns))
         (sw (if ns (display-pixel-width) 1920))
         (sh (if ns (display-pixel-height) 1200)))
    (set-frame-position (selected-frame)
                        (max 0 (/ (- sw w) 2))
                        (max 0 (/ (- sh h) 2)))))

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
