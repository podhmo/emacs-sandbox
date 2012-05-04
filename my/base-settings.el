(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(setq backup-inhibited t)
(setq max-specpdl-size 10000)
(setq max-lisp-eval-depth 10000)
(setq gc-cons-threshold (* 30 gc-cons-threshold))
(setq message-log-max 10000)
(setq history-length 1000)
(setq enable-recursive-minibuffers t)
(setq large-file-warning-threshold (* 25 1024 1024))

(add-hook 'server-switch-hook 
          (lambda ()
            (local-set-key (kbd "C-x k") 
                           (ilambda
                            (if server-buffer-clients
                                (server-edit)
                                (kill-buffer))))
            ))

(setq use-dialog-box nil)
(defalias 'message-box 'message)
(defalias  'yes-or-no-p 'y-or-n-p)

(when (boundp 'mac-key-mode)
  (mac-key-mode 1)
  (setq mac-option-modifier 'meta))

;; editing
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

(auto-compression-mode t)
(setq x-select-enable-clipboard t)

(setq comment-style 'multi-line)

;; tab
(setq-default indent-tabs-mode nil)

;;; mouse
(mouse-wheel-mode t)
(setq mouse-wheel-follow-mouse t)

(require 'font-lock)
(global-font-lock-mode t)

;;show line number
(require 'linum)
(global-linum-mode t)

;;tramp
(require 'tramp)

;; lang
(setq initial-major-mode 'emacs-lisp-mode)
