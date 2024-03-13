;;; -*- coding: utf-8; lexical-binding: t -*-

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

;; disable welcome message
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; load individual library
(load-file (concat (current-directory) "macros.el"))
(load-file (concat  (current-directory) "auto-save-buffers.el")) ; todo byte-compile

;; settings
(progn
  (progn ; backup handling
    (setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))) ; backup is <filename>~
    )
  
  (setq echo-keystrokes 0.2)

  (show-paren-mode 1)
  (transient-mark-mode t)

  (setq search-highlight t)
  (setq query-replace-highlight t)

  (auto-image-file-mode t)
  (setq resize-mini-windows t)

  (progn ; emacs client
    (condition-case err
	(progn
          (autoload 'server-running-p "server")
          (unless (server-running-p)  (server-start)))
      (error (message "emacsclient load fail")))
    )

  )

;; layout
(progn 
  (progn ; window-layout (frame layout in emacs's glossary)
    (pcase system-type
      ('darwin
       (dolist (x '((top . 90) (left . 80) (width . 176) (height . 41)))
	 (add-to-list 'default-frame-alist x))
       )
      ('gnu/linux ; wsl
       ;; window
       (dolist (x '((top . 0) (left . 80) (width . 176) (height . 39) ))
	 (add-to-list 'default-frame-alist x))
       ;; emoji display
       ;; need: apt-get install fonts-noto
       ;; see: https://ianyepan.github.io/posts/emacs-emojis/
       (and-let* ((its (member "Noto Color Emoji" (font-family-list))))
	 (custom-set-faces
	  '(default ((t (:family "Noto Sans CJK JP" :foundry "GOOG" :slant normal :weight normal :height 120 :width normal)))))
	 (set-fontset-font t 'symbol (font-spec :family (car its)) nil 'prepend))
       )
      )
    )

  ;; theme
  (let ((available-themes (custom-available-themes))
	(themes '(modus-vivendi tango-dark))) ; theme
    (cl-dolist (theme themes)
      (when (member theme available-themes)
	(message "-- load theme -- %s --" theme)
	(load-theme theme)
	(cl-return))))

  (progn ; line number
    (global-display-line-numbers-mode t)
    (custom-set-variables '(display-line-numbers-width-start t))
    )
  
  (progn ; mode-line
    (column-number-mode t)
    (display-time-mode t)
    )

  ;; (menu-bar-mode)
  (tool-bar-mode -1)

  (progn ; tab-bar
    (tab-bar-mode 1)
    (global-set-key (kbd "C-c C-n") 'tab-next)
    (global-set-key (kbd "C-c C-p") 'tab-previous)
    (global-set-key (kbd "C-c C-f") 'find-file-other-tab)
    (global-set-key (kbd "C-;") 'tab-next)
    )
  )

;; external
(progn
  (progn ; lisp-mode
    (setq initial-major-mode 'emacs-lisp-mode)
    )

  (progn ; javascript-mode
    (add-to-list  'auto-mode-alist '("\\.mjs" .  js-mode))
    )

  (progn ; shell
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    )
  )

;; main
(progn
  (progn ; auto-save
    (def-toggle auto-save-buffers-toggle
      (:on (auto-save-buffer-activate))
      (:off (auto-save-buffer-deactivte)))

    (auto-save-buffers-start 0.5)
    )

  (progn ; key-binding
    (defvar my:emacs-home-directory (current-directory))
    (global-set-key (kbd "C-c x") (lambda () (interactive)
				    (let ((file (concat my:emacs-home-directory "init.el")))
				      (if (fboundp 'switch-to-buffer-other-tab)
					  (switch-to-buffer-other-tab file)
					(find-file file)))
				    ))
    )

  ;; open memo file
  (pcase system-type
    ('darwin
     (let* ((cmd "ls -t ~/vboxshare/memo/memo*.txt | head -n 1")
	    (memo-file (replace-regexp-in-string "\n" ""  (shell-command-to-string cmd))))
       (find-file memo-file)))
    ('gnu/linux ; wsl
     (let* ((cmd "ls -t /mnt/c/Users/nao/vboxshare/memo/memo*.txt | head -n 1")
	    (memo-file (replace-regexp-in-string "\n" ""  (shell-command-to-string cmd))))
       (find-file memo-file)))
    (typ (message "default text file is not found in system-type='%S" typ))
    )
  )

