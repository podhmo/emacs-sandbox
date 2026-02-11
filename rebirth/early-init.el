;; disable welcome message
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(push '(tool-bar-lines . 0) default-frame-alist)

(progn ;; window-layout
  (defun my:setup-layout--mac ()
    (setq-default line-spacing 0.03) ;; すこしだけ行間にスペースをいれる

    (dolist (x '((top . 90) (left . 80) (width . 176) (height . 41) (left-fringe 12) (right-fringe 12)))
      (add-to-list 'default-frame-alist x)) )

  (defun my:setup-layout--windows ()
    (setq-default line-spacing 0.03) ;; すこしだけ行間にスペースをいれる

    (dolist (x '((top . 0) (left . 80) (width . 176) (height . 39) (left-fringe 12) (right-fringe 12)))
      (add-to-list 'default-frame-alist x))
    ;; emoji display
    ;; need: apt-get install fonts-noto
    ;; see: https://ianyepan.github.io/posts/emacs-emojis/
    (and-let* ((its (member "Noto Color Emoji" (font-family-list))))
      (custom-set-faces
       '(default ((t (:family "Noto Sans CJK JP" :foundry "GOOG" :slant normal :weight normal :height 120 :width normal)))))
      (set-fontset-font t 'symbol (font-spec :family (car its)) nil 'prepend))
    )

  ;; window-layout (frame layout in emacs's glossary)
  (pcase system-type
    ('darwin
     (my:setup-layout--mac))
    ('gnu/linux ; wsl
     (my:setup-layout--windows ))
    (typ (message "layout setting is not found. system-type='%S" typ))
    )
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
