;;; -*- coding: utf-8; lexical-binding: t -*-

(setq debug-on-error t) ;; enable for debugging

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

;; ignore custom.el
(setq custom-file (concat (current-directory) "custom.el"))

;; load individual library
(load-file (concat (current-directory) "macros.el"))

;; settings
(progn
  (progn ; for performance
    (setq gc-cons-threshold (* 32 1024 1024)) ;; 32mb
    )

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
              (setq-local auto-save-visited-mode nil)
              (cl-return nil))))))
    (add-hook 'find-file-hook 'my:find-file-hook--disable-auto-save)
    )

  ;; case sensitive/insensitive
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)


  (global-auto-revert-mode t)
  (setq echo-keystrokes 0.2)

  (setq visible-bell t) ;; disable beep

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
  ;; (add-hook 'tab-bar-tab-post-open-functions 'my:tab-bar-open-hook--for-dedup)
  ;; (remove-hook 'tab-bar-tab-post-open-functions 'my:tab-bar-open-hook--for-dedup)

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
  (defalias 'my:dedup-tabs 'my:tab-bar-dedup-tabs)

  ;; (tab-bar-history-mode 1)
  (defun my:find-file-with-tab-bar--server-visit-hook ()
    "単純にemacsclientで開いたときにタブで開く。重複したら手動で `my:dedup-tabs' を呼ぶ"
    (find-file-other-tab (buffer-file-name)))
  (add-hook 'server-visit-hook 'my:find-file-with-tab-bar--server-visit-hook)  ;; emacsclientでは常にnew-tabでファイルを開く
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

  (progn ; make-mode
    (with-eval-after-load 'make-mode
      ;; auto-save中で定期的にy-or-n-pで尋ねてくるのはうるさすぎるので *Messages* に書くだけにする
      (defun makefile-warn-suspicious-lines ()
        ;; Returning non-nil cancels the save operation
        (if (derived-mode-p 'makefile-mode)
            (save-excursion
	      (goto-char (point-min))
	      (if (re-search-forward "^\\(\t+$\\| +\t\\)" nil t)
	          (message "Suspicious line %d. Save anyway? "
			   (count-lines (point-min) (point)))))))
      (defun makefile-warn-continuations ()
        (if (derived-mode-p 'makefile-mode)
            (save-excursion
	      (goto-char (point-min))
	      (if (re-search-forward "\\\\[ \t]+$" nil t)
		  (message "Suspicious continuation in line %d. Save anyway? "
			   (count-lines (point-min) (point)))))))
      ))

  (progn ; javascript-mode
    (add-to-list  'auto-mode-alist '("\\.mjs" .  js-mode))

    (with-eval-after-load 'js
      ;; 対応するペアの出力
      (add-hook 'js-mode-hook 'electric-pair-mode)

      ;; 1行が長大なファイルへの対応
      (defun my:count-chars-of-first-line ()
        (interactive)
        (save-excursion
          (save-restriction
            (goto-char (point-min))
            (let (beg end)
              (beginning-of-line)
              (setq beg (point))
              (end-of-line)
              (setq end (point))
              (when (interactive-p)
                (message "first chars is %s beg=%s end=%s" (- end beg) beg end))
              (- end beg)))))

      (defun my:open-with-low-cost-mode--if-huge-first-line ()
        (interactive)
        "先頭行が長過ぎる場合に、論理行での移動を止める"
        (let ((threashold 2000))
          (when (>= (my:count-chars-of-first-line) threashold)
            (fundamental-mode)
            (message "huge first line, so setq-local line-mode-visual nil")
            (setq-local line-move-visual nil) ;; C-nでの移動は論理行ではなく物理行にする
            )))

      (add-hook 'js-json-mode-hook 'my:open-with-low-cost-mode--if-huge-first-line))
    )

  (progn ; shell
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

    (defun my:shell-command-on-region-with-kill-new (start end command &optional output-buffer replace error-buffer display-error-buffer region-noncontiguous-p)
      "`shell-command-on-region'のkill-ringに追加する版"
      ;; ここはshell-command-on-regionのinteractiveのコードそのまま
      (interactive (let (string)
                     (unless (mark)
		       (user-error "The mark is not set now, so there is no region"))
		     (setq string (read-shell-command "Shell command on region: "))
		     (list (region-beginning) (region-end) string current-prefix-arg current-prefix-arg shell-command-default-error-buffer t (region-noncontiguous-p))))

      (let ((out-buffer-name shell-command-buffer-name))
        (shell-command-on-region start end command output-buffer replace out-buffer-name out-buffer-name region-noncontiguous-p)
        (unless replace
          (let ((output-string
                 (with-current-buffer shell-command-buffer-name
                   (buffer-substring (point-min) (point-max))
                   )))
            (message output-string)
            (kill-new output-string)))))
    )

  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "C-c C-p") 'tab-previous)
    (define-key python-mode-map (kbd "C-c C-n") 'tab-next)
    )

  (progn ; markdown
    (unless (locate-library "markdown-mode")
      (add-to-list  'auto-mode-alist '("\\.md" .  text-mode))
      )
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

      ;; text editing
      (define-key text-mode-map (kbd "C-M-i") 'dabbrev-expand)

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

  (defun my:ansi-color-strip ()
    "for github action's log .etc"
    (interactive)
    (save-excursion
      (while (re-search-forward "\\\\[[0-9]*m" nil t 1)
        (replace-match ""))))

  (autoload 'ansi-color-apply-on-region "ansi-color")
  (defun my:ansi-color-highlight ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
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

(progn ;; view-mode
  (setq view-read-only t) ;; read-onlyのときにはview-modeで開く
  (with-eval-after-load 'view
    (defun my:dired-current-directory () (interactive)
           (dired (current-directory)))
    (defun my:view-mode-setup ()
      (hl-line-mode 1)

      ;; hjkl
      (define-key view-mode-map (kbd "h") 'backward-char)
      (define-key view-mode-map (kbd "j") 'next-line)
      (define-key view-mode-map (kbd "k") 'previous-line)
      (define-key view-mode-map (kbd "l") 'forward-char)

      ;; page-up/page-down
      (define-key view-mode-map (kbd "J") 'scroll-up)
      (define-key view-mode-map (kbd "K") 'scroll-down)

      ;; todo: next-definition/previous-definition

      ;; directory
      (define-key view-mode-map (kbd "[") 'my:dired-current-directory)
      )
    (defun my:view-mode-cleanup ()
      (hl-line-mode -1))
    (add-hook 'view-mode-on-hook 'my:view-mode-setup)
    (add-hook 'view-mode-off-hook 'my:view-mode-cleanup)
    )

  (cl-defun my:find-file-hook--enable-view-mode (&key (writable-modes '(text-mode markdown-mode emacs-lisp-mode)))
    "text-mode, markdown-mode以外の場合にはview-modeで開く"
    (unless(memq major-mode writable-modes)
      (view-mode 1)))
  (add-hook 'find-file-hook 'my:find-file-hook--enable-view-mode)

  (with-eval-after-load 'help-mode
    (defun my:help-mode-setup ()
      (define-key help-mode-map (kbd "[") 'help-go-back)
      (define-key help-mode-map (kbd "]") 'help-go-forward)
      )
    (add-hook 'help-mode-hook 'my:help-mode-setup)
    )
  )



(cl-defun side-pocket:toggle-filename (fname &key (marker "tmp"))
  "toggle file name.  e.g. foo.txt <-> foo.tmp.txt"
  (let* ((parts (split-string (file-name-nondirectory fname) "\\." t))
         (new-name (string-join (cl-remove-if (lambda (x) (string-equal x marker)) parts) ".")))
    (cond ((not (null (member marker parts))) (concat (file-name-directory fname) new-name)) ;; e.g. foo.txt -> foo.tmp.txt
          (t (concat (file-name-directory fname)  (file-name-sans-extension new-name) "." marker "."  (file-name-extension new-name)))))) ;; e.g. foo.tmp.txt -> foo.txt
(defun side-pocket:toggle-buffer () (interactive)
       (when-let ((fname (buffer-file-name)))
         (find-file  (side-pocket:toggle-filename fname :marker "tmp"))))

;; main
(progn ; key-binding

  (progn ;; kill-buffer with tab-bar
    (defun my:kill-buffer-with-tab-close-if-need (&optional keep-tab-bar-p)
      "This is a wrapper of `kill-buffer'. If this function called with C-u prefix, then keeping tab (usually closing tab)"
      (interactive (list current-prefix-arg))
      (kill-buffer (current-buffer))
      (unless keep-tab-bar-p
        (tab-close)
        )
      )
    (global-set-key (kbd "C-x k") 'my:kill-buffer-with-tab-close-if-need)
    (global-set-key (kbd "C-x K") 'my:tab-bar-dedup-tabs)
    )

  (defvar my:emacs-home-directory (current-directory))
  (global-set-key (kbd "C-c x") (lambda () (interactive)
				  (let ((file (concat my:emacs-home-directory "init.el")))
				    (if (fboundp 'switch-to-buffer-other-tab)
					(find-file-other-tab file)
				      (find-file file)))
				  ))
  ;; find-file
  (global-set-key (kbd "C-x C-f") 'find-file-at-point)
  (global-set-key (kbd "C-x C-a") 'revert-buffer)
  (global-set-key (kbd "C-c C-c") 'side-pocket:toggle-buffer)

  ;; goto
  (global-set-key (kbd "C-x C-l") 'goto-line)
  ;; 標準の設定ではM-g g,  M-g nはnext-error, M-g pはprevious-error

  ;; comment
  (global-set-key (kbd "C-c q") 'comment-region)
  (global-set-key (kbd "C-c Q") 'uncomment-region)

  ;; string edit
  (global-set-key (kbd "C-c d") 'my:delete-something)
  (global-set-key (kbd "C-c e") 'my:enclose-quote)
  (global-set-key (kbd "M-r") 'replace-string)
  (global-set-key (kbd "M-R") 'replace-regexp)

  ;; undo/redo
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-.") 'undo-redo)

  ;; shell-command-region
  (global-set-key (kbd "M-|") 'my:shell-command-on-region-with-kill-new)

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
    (define-key ctrl-j-map (kbd "C-f") 'find-file-other-tab)
    (define-key ctrl-j-map "f" 'find-file-other-tab)

    (define-key ctrl-j-map (kbd "C-j") 'dabbrev-expand)

    ;; activate ctr-j map
    (global-set-key (kbd "C-j") ctrl-j-map)

    (defun my:view-mode-setup--activate-ctrl-j-map ()
      (define-key view-mode-map (kbd "C-j") ctrl-j-map))
    (add-hook 'view-mode-on-hook 'my:view-mode-setup--activate-ctrl-j-map)
    )

  ;; remember
  (global-set-key (kbd "C-c r") 'remember)
  (global-set-key (kbd "C-c C-r") 'remember)

  (progn   ;; dired
    (defun my:dired-down-directory-or-display-file ()
      (interactive)
      (let ((fpath (dired-get-file-for-visit)))
        (cond ((file-directory-p fpath) (call-interactively 'dired-find-file))
              (t (call-interactively 'dired-display-file)))
        ))

    (defun my:dired-mode-setup ()
      ;; up/down
      (define-key dired-mode-map (kbd "[") 'dired-up-directory)
      (define-key dired-mode-map (kbd "]") 'my:dired-down-directory-or-display-file)

      ;; scroll-up/scroll-down other-window
      (define-key dired-mode-map (kbd "M-n") 'scroll-other-window)
      (define-key dired-mode-map (kbd "M-p") 'scroll-other-window-down)
      (define-key dired-mode-map (kbd "M-<") 'beginning-of-buffer-other-window)
      (define-key dired-mode-map (kbd "M->") 'end-of-buffer-other-window)
      )
    (add-hook 'dired-mode-hook 'my:dired-mode-setup)
    )
  )


(defun my:skk-previous-candidate-around-advice (fn &rest args)
  "読み取り専用バッファで C-p が SKK の候補選択ではなくカーソル移動になるようにする設定, :aroundで利用する"
  (interactive)
  (if buffer-read-only
      (call-interactively 'previous-line)
    (apply fn args)))

;; after initialize
(defun my:after-initialize--mac ()
  ;; remember
  (eval-after-load 'remember (setq remember-data-file "~/vboxshare/memo/notes"))

  ;; skk
  ;; need: install ddskk by melpa
  (when (fboundp 'skk-mode)
    (advice-add 'skk-previous-candidate :around #'my:skk-previous-candidate-around-advice) ; read only bufferでカーソル移動をするために

    (setq skk-jisyo-code "utf-8") ; jisyoのエンコーディングをutf-8にする
    (setq skk-sticky-key ";") ; sticky-shiftを使ってshiftキーの節約する
    (setq skk-egg-like-newline t) ; <enter>で改行を入力しない
    (setq skk-auto-insert-paren t)
    (setq default-input-method "japanese-skk") ; C-\

    (global-set-key (kbd "C-x j") 'skk-mode) ;; disable skk-auto-fill-mode
    (global-set-key (kbd "C-x C-j") 'skk-mode)
    ;; (global-set-key (kbd "<zenkaku-hankaku>")  'toggle-input-methodl) ;; TODO: fix

    ;; text-modeのときにははじめからskk-modeを有効にしておく
    (add-hook 'text-mode-hook 'skk-mode)

    ;; mini-bufferはskkを無効にしたい
    (defun my:skk-force-latin-mode ()
      (when skk-mode (skk-latin-mode-on)))
    (add-hook 'minibuffer-setup-hook 'deactivate-input-method)
    (add-hook 'isearch-mode-hook 'my:skk-force-latin-mode)

    ;; skkの辞書ファイルはauto-saveの対象から除外する
    (push `(string-suffix-p . ".skk-jisyo") my:disable-auto-save-visited-mode-alist)

    ;; skk-insertを潰す (代替はC-enter)
    (defun my:skk-mode-setup ()
      (define-key skk-j-mode-map (kbd "C-j") ctrl-j-map)
      (define-key skk-j-mode-map (kbd "<C-return>") 'skk-insert)
      )
    (add-hook 'skk-mode-hook 'my:skk-mode-setup)
    )

  ;; open memo*.txt
  (let* ((cmd "ls -t ~/memo/memo*.txt | head -n 1")
	 (memo-file (replace-regexp-in-string "\n" ""  (shell-command-to-string cmd))))
    (find-file memo-file)
    (end-of-buffer))
  )


(defun my:after-initialize--windows ()
  ;; remember
  (eval-after-load 'remember (setq remember-data-file "/mnt/c/Users/nao/vboxshare/memo/notes"))

  ;; skk
  ;; need: apt-get install ddskk
  (when (fboundp 'skk-mode)
    (advice-add 'skk-previous-candidate :around #'my:skk-previous-candidate-around-advice) ; read only bufferでカーソル移動をするために

    (setq skk-jisyo-code "utf-8") ; jisyoのエンコーディングをutf-8にする
    (setq skk-sticky-key ";") ; sticky-shiftを使ってshiftキーの節約する
    (setq skk-egg-like-newline t) ; <enter>で改行を入力しない
    (setq skk-auto-insert-paren t)
    (setq default-input-method "japanese-skk") ; C-\

    (global-set-key (kbd "C-x j") 'skk-mode) ;; disable skk-auto-fill-mode
    (global-set-key (kbd "C-x C-j") 'skk-mode)
    ;; (global-set-key (kbd "<zenkaku-hankaku>")  'toggle-input-methodl) ;; TODO: fix

    ;; text-modeのときにははじめからskk-modeを有効にしておく
    (add-hook 'text-mode-hook 'skk-mode)

    ;; mini-bufferはskkを無効にしたい
    (defun my:skk-force-latin-mode ()
      (when skk-mode (skk-latin-mode-on)))
    (add-hook 'minibuffer-setup-hook 'deactivate-input-method)
    (add-hook 'isearch-mode-hook 'my:skk-force-latin-mode)

    ;; skk-insertを潰す (代替はC-enter)
    (defun my:skk-mode-setup ()
      (define-key skk-j-mode-map (kbd "C-j") ctrl-j-map)
      (define-key skk-j-mode-map (kbd "<C-return>") 'skk-insert)
      )
    (add-hook 'skk-mode-hook 'my:skk-mode-setup)


    ;; skkの辞書ファイルはauto-saveの対象から除外する
    (push `(string-suffix-p . ".skk-jisyo") my:disable-auto-save-visited-mode-alist)

    ;; key binding
    (global-set-key (kbd "<muhenkan>") 'delete-backward-char)      ;; TODO: with skk
    )
  ;; open memo*.txt
  (let* ((cmd "ls -t /mnt/c/Users/nao/vboxshare/memo/memo*.txt | head -n 1")
	 (memo-file (replace-regexp-in-string "\n" ""  (shell-command-to-string cmd))))
    (find-file memo-file))
  )


;; 手抜きで "*Compile-Log*" bufferを閉じる (delete-backward-charが使われてるらしい)
(add-to-list 'display-buffer-alist
             '("^\\*Compile-Log\\*"
               (display-buffer-below-selected)
               (window-height . 0.3)
               (side . bottom)
               (inhibit-same-window . t)
               (body-function . (lambda (w)
                                  (run-at-time 3 nil 'delete-window w)))))

;; after initialize settings
(pcase system-type
  ('darwin
   (my:after-initialize--mac))
  ('gnu/linux ; wsl
   (my:after-initialize--windows))
  (typ (message "the after initialize setting is not found in system-type='%S" typ))
  )

(setq debug-on-error nil)  ;; disable in daily life

;; ;; external package
;; (setq package-archives  `(("melpa" . "https://melpa.org/packages/")  ("melpa-stable" . "https://stable.melpa.org/packages/") ,@package-archives))
;; (package-initialize)
;; (package-install 'writeroom-mode) ; -> zen-mode in vscode
;; (package-install 'ddskk-postframe);  skkの変換候補を良いかんじに表示してくる


;; ;; activate tree-sitter
;; see: M-x view-emacs-news
;; see: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; (push '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src") treesit-language-source-alist)
;; (push '(mermaid "https://github.com/monaqa/tree-sitter-mermaid" "master" "src") treesit-language-source-alist)
;; (push '(yaml "https://github.com/ikatyang/tree-sitter-yaml" "master" "src") treesit-language-source-alist)
;; (push '(markdown "https://github.com/ikatyang/tree-sitter-markdown") treesit-language-source-alist)
;;
;; (treesit-install-language-grammar 'typescript) ;; generate tree-sitter/libtree-sitter-typescript.so
;; (treesit-install-language-grammar 'mermaid)
;; (treesit-install-language-grammar 'yaml)
;; (treesit-install-language-grammar 'markdown)
;; M-x typescript-ts-mode
