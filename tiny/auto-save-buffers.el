;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'cl-lib)

(defvar auto-save-buffers-timer nil)

(defun auto-save-buffer-activate () (interactive)
       (let1 n (string-to-number (completing-read "n:" nil))
         (auto-save-buffers-start n))
       (message "auto-save-buffer is active"))

(defun auto-save-buffer-deactivte () (interactive)
       (cancel-timer auto-save-buffers-timer)
       (setq auto-save-buffers-timer nil)
       (message "auto-save-buffer is deactive"))

(def-toggle auto-save-buffers-toggle
  (:on (auto-save-buffer-activate))
  (:off (auto-save-buffer-deactivte)))

(defun auto-save-buffers-start (delay)
  (setq auto-save-buffers-timer
        (run-with-idle-timer delay t 'auto-save-buffers)))

(defvar auto-save-disable-check-functions nil)
(defun auto-save-disable-check (buf)
  (rlet1 disabled nil
    (cl-loop for p in auto-save-disable-check-functions
          if (funcall p buf)
          do (setq disabled t))))

(defun auto-save-buffers ()
  (cl-loop for buf in (cl-remove-if-not 'buffer-file-name (buffer-list))
        when (and (buffer-modified-p buf)
                  (not buffer-read-only)
                  (not (string-match "^#" (buffer-name buf)))
                  (file-writable-p (buffer-file-name buf))
                  (not (auto-save-disable-check buf)))
        do (with-current-buffer buf
             (save-buffer))))

(defun auto-save-buffer-deactivte-confirm () (interactive)
       (rlet1 status (y-or-n-p "do you deactivate auto-save timer?")
         (when status
           (auto-save-buffer-deactivte))))
