;;; -*- coding: utf-8; lexical-binding: t -*-

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



;; lisp-mode
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


;; settings
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))


					; todo byte-compile
(load-file (concat  (current-directory) "auto-save-buffers.el")) 

;; settings

(progn ;; emacsclient
  (condition-case err
      (progn
        (autoload 'server-running-p "server") 
        (unless (server-running-p)  (server-start)))
    (error (message "emacsclient load fail"))))

(auto-save-buffers-start 0.5)
