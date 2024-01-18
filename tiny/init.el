;;; -*- coding: utf-8; lexical-binding: t -*-

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

;; settings

(progn ; disable welcome message
  (setq inhibit-startup-message t)
  (setq inhibit-splash-screen t)
  )

(load-file (concat (current-directory) "macros.el"))
(load-file (concat  (current-directory) "auto-save-buffers.el")) ; todo byte-compile

(setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))) ; backup is <filename>~

(progn ;; emacsclient
  (condition-case err
      (progn
        (autoload 'server-running-p "server") 
        (unless (server-running-p)  (server-start)))
    (error (message "emacsclient load fail"))))

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

;; lisp-mode
(setq initial-major-mode 'emacs-lisp-mode)

;; shell
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

;; main
(auto-save-buffers-start 0.5)
