;;; -*- coding: utf-8; lexical-binding: t -*-

;; lang
(setq initial-major-mode 'emacs-lisp-mode)

;; eye candy
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(setq echo-keystrokes 0.2)
(column-number-mode t)
(show-paren-mode 1)
(display-time-mode t)
(transient-mark-mode t)

(setq search-highlight t)
(setq query-replace-highlight t)

(auto-image-file-mode t)
(setq resize-mini-windows t)

;; shell
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let ((,var ,val))
     ,@body))

(defmacro rlet1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let1 ,var ,val
     ,@body
     ,var))

(defmacro aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2)
           (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro def-toggle (name &rest body)
  (and-let* ((on-clause (aif (assoc-default :on body) `(progn ,@it)))
             (off-clause (aif (assoc-default :off body) `(progn ,@it)))
             (state (gensym)) (flag (gensym)))
    `(let (,state)
       (defun ,name (&optional ,flag) (interactive "P")
         (cl-case ,flag
           ((1 t) (progn ,on-clause (setq ,state t)))
           ((-1) (progn ,off-clause (setq ,state nil)))
           (otherwise (,name (if (not ,state) 1 -1))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-save-buffers 
;; TODO: mv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
