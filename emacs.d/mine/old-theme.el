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
        ,@default-frame-alist))

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

;; (unless (package-installed-p 'atom-dark-theme)
;;    (package-install 'atom-dark-theme))
;; (load-theme 'atom-dark t)

;;adhoc
(set-face-attribute 'default nil :height 140)
;;(describe-face 'font-lock-variable-name-face)
;;   (print* (frame-parameters (selected-frame)))
(set-face-attribute 'default nil :foreground "White")
(set-face-attribute 'font-lock-comment-face nil :weight 'semi-light :foreground "RosyBrown3")
;; (set-face-attribute 'help-argument-name nil :slant 'normal)
(set-face-attribute 'font-lock-string-face nil :foreground "RosyBrown3" :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :foreground "PaleGreen3" :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :foreground "SlateBlue3" :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil :foreground "plum3" :slant 'normal :weight 'bold)
(set-face-attribute 'font-lock-function-name-face nil :foreground "plum3" :weight 'bold)
(set-face-attribute 'font-lock-type-face nil :foreground "DarkSlateGray4":weight 'bold)
