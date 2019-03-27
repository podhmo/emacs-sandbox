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

;;path setting
(defun get-exec-path-from-shell ()
  (let ((output (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'")))
    (car (last (split-string output)))))

(let ((path-from-shell (get-exec-path-from-shell)))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))


;;dabbrev
(setq dabbrev-case-replace nil)
(setq dabbrev-case-distinction nil)

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

;; auto-revert
(global-auto-revert-mode t)

;;(get-language-info current-language-environment 'exit-function)
;;(get-language-info current-language-environment 'coding-priority)
;; (set-language-environment "Japanese")
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; hmm.
(setq auto-coding-functions nil)

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; hmm.
(if (equal system-type 'darwin)
    (if (>= emacs-major-version 23)
        (set-file-name-coding-system 'utf-8-nfd)
      (progn
        (require 'utf-8m)
        (set-file-name-coding-system 'utf-8m)))
  (setq file-name-coding-system 'utf-8-unix))

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
        ,@default-frame-alist))

(use-package dracula-theme
  :ensure t
  :config
  (set-face-attribute 'default nil :height 140)
  )

(let ((sysname (symbol-name system-type)))
  (cond ((string-match-p "darwin" sysname)
         ;; todo create font
         (defvar myfont "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default")
         (set-fontset-font
          myfont
          'japanese-jisx0208
          ;;                  (font-spec :family "Hiragino Kaku Gothic ProN")
          (font-spec :family "Hiragino Maru Gothic Pro"))
         (add-to-list 'default-frame-alist `(font . ,myfont))
         )
        ((string-match-p "linux" sysname)

         )
        (t )))

;; emoji
;; install symbola-font or google-noto-emoji-font and fc-cache -vf
