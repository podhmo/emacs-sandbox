;;; -*- coding: utf-8; lexical-binding: t -*-

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

;; ignore custom.el
(setq custom-file (concat (current-directory) "custom.el"))

;; disable welcome message
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; load individual library
(load-file (concat (current-directory) "macros.el"))

;; settings
(progn
  (progn ; backup handling
    (setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))) ; backup is <filename>~
    )
  (progn ; auto-save
    (setq auto-save-visited-interval 0.5)
    (auto-save-visited-mode t)


    (defvar my:disable-auto-save-visited-mode-alist nil)
    (push `(string-prefix-p . "/tmp") my:disable-auto-save-visited-mode-alist) ;; /tmp/** のファイルは自動saveしない

    (defun my:find-file-hook--disable-auto-save ()
      (let ((fname (buffer-file-name)))
        (cl-dolist (arg my:disable-auto-save-visited-mode-alist)
          (cl-destructuring-bind (fn . x) arg
            (when (funcall fn x fname)
              (message "# auto-save-visited-mode is disabled by %s %s" fn x)
              (auto-save-visited-mode -1)
              (cl-return nil))))))
    (add-hook 'find-file-hook 'my:find-file-hook--disable-auto-save)
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
  (setq-default line-spacing 0.03) ;; すこしだけ行間にスペースをいれる

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
(setq-default  indent-tabs-mode nil)

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


    (defun my:elisp-mode-setup ()
      (define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-defun)
      (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'eval-buffer)
      (define-key emacs-lisp-mode-map (kbd "C-x C-s") 'my:elisp-pretty-print-region) ;; saveはauto-save任せ
      )
    (add-hook 'emacs-lisp-mode-hook 'my:elisp-mode-setup)
    (setq initial-major-mode 'emacs-lisp-mode)

    )

  (progn ; javascript-mode
    (add-to-list  'auto-mode-alist '("\\.mjs" .  js-mode))
    )

  (progn ; shell
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    )

  (progn ; text-mode
    (defun my:indent-rigitly (n) (interactive "p")
           (setq n (or n 1))
           (if (use-region-p)
               (indent-rigidly
                (save-excursion (goto-char (region-beginning)) (point-at-bol))
                (save-excursion (goto-char (region-end)) (point-at-eol))
                (* n  4))
             (indent-rigidly (point-at-bol) (point-at-eol) (* n  4))))
    (defun my:unindent-rigitly (n) (interactive "p")
           (setq n (or n 1))
           (if (use-region-p)
               (indent-rigidly
                (save-excursion (goto-char (region-beginning)) (point-at-bol))
                (save-excursion (goto-char (region-end)) (point-at-eol))
                (* n  -4))
             (indent-rigidly (point-at-bol) (point-at-eol) (* n  -4))))
    (defun my:text-mode-setup ()
      (setq-local line-spacing 0.05) ;; ちょっと行間を拡げる

      ;; indent
      (define-key text-mode-map (kbd "<tab>")  'my:indent-rigitly)
      (define-key text-mode-map (kbd "<backtab>")  'my:unindent-rigitly)
      (define-key text-mode-map (kbd "<S-iso-lefttab>") 'my:unindent-rigitly) ;; for skk
      )
    (add-hook 'text-mode-hook 'my:text-mode-setup)
    )

  )

(progn ;; text-editing
  (defun my:delete-something () (interactive)
         (cl-dolist (thing '(symbol word whitespace))
	   (when-let ((found (thing-at-point thing)))
	     (cl-return (delete-region (beginning-of-thing thing)  (end-of-thing thing ))))))

  (defun my:enclose-quote (beg end)
    "foo -> \"foo\""
    (interactive
     (list
      (if (use-region-p) (region-beginning) (beginning-of-thing 'word))
      (if (use-region-p) (region-end) (end-of-thing 'word))))
    (save-restriction
      (let ((text (buffer-substring-no-properties beg end)))
        (delete-region beg end)
        (insert (prin1-to-string text))))  ;; prin1-to-string ha tenuki
    )
  )

(progn ;; code reading
  (cl-defun my:browse-github (&key (branch nil) (rel-path nil) (line-no nil))
    (interactive)
    (and-let*
        ((root  (vc-git-root default-directory))
         (url (my:resolve-github-url  (format "%s.git/config" root ))))
      (let* ((branch (or branch (car (vc-git-branches))))
             (rel-path (or rel-path (replace-regexp-in-string (expand-file-name  root) "" (buffer-file-name))))
             (line-no (or line-no (line-number-at-pos)))
             (full-url (format "%s/blob/%s/%s#L%d" url branch rel-path line-no)))
        (browse-url full-url))))

  (defun my:resolve-github-url (git-config-file)
    (cl-block b
      (let ((buf (find-file-noselect git-config-file)))
        (with-current-buffer buf
          (goto-char (point-min))
          (when (re-search-forward "url = git@github.com:\\(.+\\)\\(?:\\.git\\)?" nil t)
            (let ((user-repository-name (string-trim-right (match-string-no-properties 1) "\\.git$")))
              (cl-return-from b (format "https://github.com/%s" user-repository-name))))
          (goto-char (point-min))
          (when (re-search-forward "url = ssh://git@github.com/\\(.+\\)\\(?:\\.git\\)?" nil t)
            (let ((user-repository-name (string-trim-right (match-string-no-properties 1) "\\.git$")))
              (cl-return-from b (format "https://github.com/%s" user-repository-name))))
          (goto-char (point-min))
          (when (re-search-forward "url = https://github.com/\\(.+\\)\\(?:\\.git\\)?" nil t)
            (let ((user-repository-name (string-trim-right (match-string-no-properties 1) "\\.git$")))
              (cl-return-from b (format "https://github.com/%s" user-repository-name))))
          ))))

  (defalias 'browse-github 'my:browse-github)
  (defun browse-github-master () (interactive)
         (browse-github :branch "master"))
  (defun browse-github-main () (interactive)
         (browse-github :branch "main"))
  (defun browse-github-develop () (interactive)
         (browse-github :branch "develop"))
  )

;; main
(progn
  (progn ; key-binding

    (defvar my:emacs-home-directory (current-directory))
    (global-set-key (kbd "C-c x") (lambda () (interactive)
				    (let ((file (concat my:emacs-home-directory "init.el")))
				      (if (fboundp 'switch-to-buffer-other-tab)
					  (my:find-file-or-switch-buffer-other-tab file)
					(find-file file)))
				    ))
    (global-set-key (kbd "C-x C-f") 'find-file-at-point)

    ;; comment
    (global-set-key (kbd "C-c q") 'comment-region)
    (global-set-key (kbd "C-c Q") 'uncomment-region)

    ;; string edit
    (global-set-key (kbd "C-c d") 'my:delete-something)
    (global-set-key (kbd "C-c e") 'my:enclose-quote)
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

      (define-key ctrl-j-map (kbd "C-j") 'dabbrev-expand)

      (global-set-key (kbd "C-j") ctrl-j-map) ; activate
      )
    )

  ;; after initialize
  (pcase system-type
    ('darwin

     (progn   ;; remember
       (global-set-key (kbd "C-c r") 'remember)
       (global-set-key (kbd "C-c C-r") 'remember)
       (eval-after-load 'remember (setq remember-data-file "~/vboxshare/memo/notes"))
       )

     ;; open memo*.txt
     (let* ((cmd "ls -t ~/vboxshare/memo/memo*.txt | head -n 1")
	    (memo-file (replace-regexp-in-string "\n" ""  (shell-command-to-string cmd))))
       (find-file memo-file)))

    ('gnu/linux ; wsl

     (progn   ;; remember
       (global-set-key (kbd "C-c r") 'remember)
       (global-set-key (kbd "C-c C-r") 'remember)
       (eval-after-load 'remember (setq remember-data-file "/mnt/c/Users/nao/vboxshare/memo/notes"))
       )

     (progn ;; skk
       ;; need: apt-get install ddskk
       ;; M-x  skk-get with encoding=euc-jp
       (when (fboundp 'skk-mode)
         (setq skk-egg-like-newline t) ; <enter>で改行を入力しない
         (setq skk-auto-insert-paren t)
         (setq default-input-method "japanese-skk") ; C-\

         (global-set-key (kbd "C-x j") 'skk-mode) ;; disable skk-auto-fill-mode
         (global-set-key (kbd "C-x C-j") 'skk-mode)
         ;; (global-set-key (kbd "<zenkaku-hankaku>")  'toggle-input-methodl) ;; TODO: fix

         ;; skkの辞書ファイルはauto-saveの対象から除外する
         (push `(string-suffix-p . ".skk-jisyo") my:disable-auto-save-visited-mode-alist)
         )
       )

     ;; key binding
     (global-set-key (kbd "<muhenkan>") 'delete-backward-char)      ;; TODO: with skk

     ;; open memo*.txt
     (let* ((cmd "ls -t /mnt/c/Users/nao/vboxshare/memo/memo*.txt | head -n 1")
	    (memo-file (replace-regexp-in-string "\n" ""  (shell-command-to-string cmd))))
       (find-file memo-file)))
    (typ (message "default text file is not found in system-type='%S" typ))
    )
  )

;; ;; external package
;; (setq package-archives  `(("melpa" . "https://melpa.org/packages/")  ("melpa-stable" . "https://stable.melpa.org/packages/") ,@package-archives))
;; (package-initialize)
;; (package-install 'writeroom-mode) ; -> zen-mode in vscode
