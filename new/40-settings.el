
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
;;(mouse-wheel-mode t)
;;(setq mouse-wheel-follow-mouse t)

(require 'font-lock)
(global-font-lock-mode t)

;;show line number
(require 'linum)
(global-linum-mode t)

;;tramp
(require 'tramp)

;; lang
(setq initial-major-mode 'emacs-lisp-mode)

;; eye candy
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(setq echo-keystrokes 0.2)
(line-number-mode t)
(column-number-mode t)
(show-paren-mode 1)
(display-time-mode t)
(transient-mark-mode t)

(setq search-highlight t)
(setq query-replace-highlight t) 

(auto-image-file-mode t)
(setq resize-mini-windows t) 

(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

;; indent
(setq default-tab-width 4)
(setq indent-line-function 'indent-relative-maybe)
(setq-default default-tab-width 4
              tab-width 4
              indent-tabs-mode nil)
;; (set-default 'tab-stop-list 
;;                (loop for i from 4 to 120 by 4
;;                      collect i))


(setq default-frame-alist
      `(
        ;; (top . 10) 
        ;; (left . 10)
      	;; (width . 106)
      	;; (height . 40)
        (frame-cursor-color . "steelblue")
        (scroll-bar-background . "grey75")
        (scroll-bar-foreground)
        (border-color . "black")
        (cursor-color . "Steelblue")
        (mouse-color . "gold")
        ;; (background-color . "black")
        (background-color . "dark slate gray")
        (foreground-color . "white")
        (font . "-apple-Menlo-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1" )
        ,@default-frame-alist))

;;(describe-face 'font-lock-variable-name-face)
;;   (print* (frame-parameters (selected-frame)))
(set-face-attribute 'font-lock-comment-face nil :weight 'semi-light :foreground "RosyBrown3")
;; (set-face-attribute 'help-argument-name nil :slant 'normal)
(set-face-attribute 'font-lock-string-face nil :foreground "RosyBrown3" :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :foreground "PaleGreen3" :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :foreground "SlateBlue3" :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil :foreground "plum3" :slant 'normal :weight 'bold)
(set-face-attribute 'font-lock-function-name-face nil :foreground "plum3" :weight 'bold)
(set-face-attribute 'font-lock-type-face nil :foreground "DarkSlateGray4":weight 'bold)
