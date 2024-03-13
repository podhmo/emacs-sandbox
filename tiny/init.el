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

  (global-auto-revert-mode t)
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
  )

(progn ; tab-bar
  (tab-bar-mode 1)
  (global-set-key (kbd "C-c C-n") 'tab-next)
  (global-set-key (kbd "C-c C-p") 'tab-previous)
  (global-set-key (kbd "C-c C-f") 'find-file-other-tab)
  (global-set-key (kbd "C-;") 'tab-next)

  (defun my:tab-bar-open-hook--for-dedup (tab)
    (run-with-timer 0.1 nil 'my:tab-bar-dedup-tabs)) ; work-around
  (defun my:tab-bar-open-hook--for-debug (tab)
    (message "tab: %s -- all tabs %s" (alist-get 'name tab) (tab-bar-tab-name-all)))

  ;; side-effect
  (add-hook 'tab-bar-tab-post-open-functions 'my:tab-bar-open-hook--for-debug)
  (add-hook 'tab-bar-tab-post-open-functions 'my:tab-bar-open-hook--for-dedup)

  (defun my:tab-bar-dedup-tabs () (interactive)
	 (let ((visited nil)
	       (removed nil)
	       (current-tab-name (tab-bar-tab-name-current)))
	   (dolist (tab (funcall tab-bar-tabs-function))
	     (let ((tab-name (alist-get 'name tab)))
	       (if (member tab-name visited) (push tab-name removed) (push tab-name visited))))
	   ;; close tabs
	   (dolist (tab-name removed)
	     (tab-bar-close-tab-by-name tab-name))
	   ;; select tab by name
	   (tab-bar-select-tab-by-name current-tab-name)
	   ))

  (defun my:find-file-or-switch-buffer-other-tab (name)  (interactive "f")
	 (cond ((string-equal name "")  (tab-bar-new-tab))
	       (t    (cl-dolist (b (buffer-list))
		       (when (string-equal name (buffer-file-name b))
			 (cl-dolist (tab (funcall tab-bar-tabs-function))
			   (when (string-equal name (alist-get 'name tab) )
			     (cl-return (tab-bar-select-tab-by-name name))))
			 (cl-return (switch-to-buffer-other-tab name))))
		     (find-file-other-tab name ))))
  )

;; external
(progn
  (progn ; lisp-mode
    (defun my:elisp-pretty-print-region (beg end)
      (interactive
       (list
	(if (use-region-p) (region-beginning) (point-min))
	(if (use-region-p) (region-end) (point-max))))
      (save-excursion
	(unwind-protect
	    (progn
	      (narrow-to-region beg end)
	      (goto-char (point-min))
	      (while (re-search-forward "[  	]+$" nil t 1)
		(replace-match ""))
	      (indent-region (point-min) (point-max)))
	  (widen))))

    (setq initial-major-mode 'emacs-lisp-mode)
    )

  (progn ; javascript-mode
    (add-to-list  'auto-mode-alist '("\\.mjs" .  js-mode))
    )

  (progn ; shell
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    )
  )

(defun my:delete-something () (interactive)
       (cl-dolist (thing '(symbol word))
	 (when-let ((found (thing-at-point thing)))
	   (cl-return (delete-region (beginning-of-thing thing)  (end-of-thing thing ))))))

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
					  (my:find-file-or-switch-buffer-other-tab file)
					(find-file file)))
				    ))

    ;; comment
    (global-set-key (kbd "C-c q") 'comment-region)
    (global-set-key (kbd "C-c Q") 'uncomment-region)

    ;; string edit
    (global-set-key (kbd "C-c d") 'my:delete-something)
    (global-set-key (kbd "M-r") 'replace-string)
    (global-set-key (kbd "M-R") 'replace-regexp)

    (progn    ;; ctrl-j map
      (defvar ctrl-j-map (make-keymap))
      (define-key ctrl-j-map "c" (lambda () (interactive) (switch-to-buffer-other-tab "*scratch*"))) ; tab-new
      (define-key ctrl-j-map "b" 'switch-to-buffer-other-tab)
      (define-key ctrl-j-map "n" 'tab-next)
      (define-key ctrl-j-map (kbd "C-n") 'tab-next)
      (define-key ctrl-j-map "p" 'tab-previous)
      (define-key ctrl-j-map (kbd "C-p") 'tab-previous)
      (define-key ctrl-j-map (kbd "RET") 'tab-switcher)
      (define-key ctrl-j-map "r" 'tab-rename)
      (define-key ctrl-j-map "k" 'tab-close)
      (define-key ctrl-j-map "K" 'my:tab-bar-dedup-tabs)
      (define-key ctrl-j-map "m" 'tab-bar-move-tab-to) ; e.g. C-u 1 C-j m
      (define-key ctrl-j-map (kbd "C-f") 'my:find-file-or-switch-buffer-other-tab)
      (define-key ctrl-j-map "f" 'my:find-file-or-switch-buffer-other-tab)
      (global-set-key (kbd "C-j") ctrl-j-map) ; activate
      )
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
