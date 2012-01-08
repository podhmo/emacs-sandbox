;; eye candy
(menu-bar-mode -1)
(tool-bar-mode -1)
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
        (frame-background-color . "black")
        (frame-foreground-color . "white")
        (frame-cursor-color . "steelblue")
        (scroll-bar-background . "grey75")
        (scroll-bar-foreground)
        (border-color . "black")
        (cursor-color . "Steelblue")
        (mouse-color . "gold")
        (background-color . "black")
        (foreground-color . "white")
        ,@default-frame-alist))

(set-frame-parameter (selected-frame) 'foreground-color "white")
(set-frame-parameter (selected-frame) 'background-color "black")
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
