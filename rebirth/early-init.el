;; disable welcome message
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)


;; performance
(setq gc-cons-threshold most-positive-fixnum          ; GCを極力遅らせる
      gc-cons-percentage 0.8
      read-process-output-max (* 1024 1024 32)         ; 32MiB
      inhibit-compacting-font-caches t)

;; (add-hook 'after-init-hook
;;           (lambda () (setq gc-cons-threshold 800000))) ; 通常に戻す


(push '(tool-bar-lines . 0) default-frame-alist)


(defun my:setup-layout--mac ()
  (setq-default line-spacing 0.03) ;; すこしだけ行間にスペースをいれる

  (dolist (x '((top . 90) (left . 80) (width . 176) (height . 41) (left-fringe 12) (right-fringe 12)))
    (add-to-list 'default-frame-alist x))
  )


(defun my:setup-layout--windows ()
  ;; need: sudo apt install fonts-noto-color-emoji fonts-noto-cjk-extra fonts-cascadia-code
  (setq-default line-spacing 0.03) ;; すこしだけ行間にスペースをいれる

  (dolist (x '((top . 0) (left . 80) (width . 176) (height . 50) (left-fringe 15) (right-fringe 15)))
    (add-to-list 'default-frame-alist x))


  ;; 1. ASCII / ラテン文字などを等幅フォントに
  (when (member "Cascadia Mono" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Cascadia Mono"      ; 好みで "DejaVu Sans Mono" / "Fira Code" / "Hack" / "Cascadia Mono" などに変更可
                        :height 128
                        :weight 'normal)
    )


  ;; 2. 日本語（CJK）を Noto Sans CJK JP で上書き
  (when (member "Noto Sans Mono CJK JP" (font-family-list))
    (set-fontset-font t 'han          "Noto Sans Mono CJK JP" nil 'prepend)
    (set-fontset-font t 'kana         "Noto Sans Mono CJK JP" nil 'prepend)
    (set-fontset-font t 'cjk-misc     "Noto Sans Mono CJK JP" nil 'prepend)
    ;; 句読点・全角スペースなどもNotoに統一（幅ずれ防止）
    (set-fontset-font t '(#x3000 . #x303f) "Noto Sans Mono CJK JP" nil 'prepend)
    )

  ;; 3. 必要なら Symbol（絵文字など）も設定
  ;; see: https://ianyepan.github.io/posts/emacs-emojis/
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
    )
  )

;; window-layout (font-family-list) の設定がframeを作成したあとじゃないとみつからないので遅延させる。
(pcase system-type
  ('darwin
   (add-hook 'after-init-hook #'my:setup-layout--mac))
  ('gnu/linux ; wsl
   (add-hook 'after-init-hook #'my:setup-layout--windows))
  (typ (message "layout setting is not found. system-type='%S" typ))
  )


(progn ;; layout
  (progn  ;; theme
    (let ((theme 'modus-vivendi))
      (message "-- load theme -- %s --" theme)
      (load-theme theme))

    ;; inactiveなタブとactiveなタブの差がわかりづらかったので暗くする (for tab-bar)
    (custom-set-faces  '(tab-bar-tab-inactive ((t (:strike-through t :inherit modus-themes-tab-inactive)))))
    )

  (progn ; line number
    (global-display-line-numbers-mode t)
    (custom-set-variables '(display-line-numbers-width-start t))
    )

  (progn ; mode-line
    (column-number-mode t)
    (display-time-mode t)
    )
  )


(setq frame-inhibit-implied-resize t)
