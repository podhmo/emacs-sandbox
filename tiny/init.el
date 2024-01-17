;;; -*- coding: utf-8; lexical-binding: t -*-

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(load-file (concat (current-directory) "macros.el"))


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
