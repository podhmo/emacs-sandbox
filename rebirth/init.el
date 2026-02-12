;;; -*- coding: utf-8; lexical-binding: t -*-

(setq debug-on-error t) ;; enable for debugging

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

;; ignore custom-settings.el
(setq custom-file (concat (current-directory) "custom-settings.el"))


;; load individual library
(eval-and-compile
  (defvar my:local-lisp-dir (expand-file-name "lisp" (current-directory))))

(use-package macros
  :load-path my:local-lisp-dir
  :demand t; 即時ロードしたい
  )
(use-package utils
  :load-path my:local-lisp-dir
  :demand t; 即時ロードしたい
  )


;; settings
(progn
  (progn ; yes/no -> y/n
    (setq use-short-answers t
          confirm-kill-emacs 'y-or-n-p))


  (progn ; text edithing
    (setq-default indent-tabs-mode nil
                  tab-width 4
                  fill-column 120))

  (progn ; backup handling
    (setq delete-by-moving-to-trash t           ; OSのゴミ箱へ
          make-backup-files nil                 ; ~ファイルを作らない
          auto-save-default nil
          create-lockfiles nil)

    ;; backup is <filename>~. make-backup-filesで無効にしてるから無駄な設定かも
    (setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
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

(progn ;; 最近使ったファイル
  (recentf-mode 1)
  (setq recentf-max-saved-items 100
        recentf-exclude '("/tmp/" "/ssh:"))
  )

(use-package languages ; TODO: 雑な定義を辞める
  :load-path my:local-lisp-dir
  :demand t; 即時ロードしたい
  )


(progn ; lisp-mode
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

  (with-eval-after-load 'js
    ;; 対応するペアの出力
    (add-hook 'js-mode-hook 'electric-pair-mode)

    ;; 1行が長大なファイルへの対応
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

  ;; M-x で *shell* bufferを開くときにはother windowに開く
  (add-to-list 'display-buffer-alist
               '("\\*shell\\*"
                 (display-buffer-at-bottom)
                 (reusable-window . t))
               )
  )

(progn ; markdown
  (unless (locate-library "markdown-mode")
    (add-to-list  'auto-mode-alist '("\\.md" .  text-mode))
    )
  )

(progn ; text-mode
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


(use-package interactives
  :load-path my:local-lisp-dir
  :demand t; 即時ロードしたい
  )

(progn ; tab-line
  (global-tab-line-mode t)
  (global-set-key (kbd "C-c <muhenkan>") 'my:tab-line-prev-tab)
  (global-set-key (kbd "C-c C-p") 'my:tab-line-prev-tab)
  (global-set-key (kbd "C-c <henkan>") 'my:tab-line-next-tab)
  (global-set-key (kbd "C-c C-n") 'my:tab-line-next-tab)
  )


(progn ;; code reading
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

  (with-eval-after-load 'help-mode
    (defun my:help-mode-setup ()
      (define-key help-mode-map (kbd "[") 'help-go-back)
      (define-key help-mode-map (kbd "]") 'help-go-forward)
      )
    (add-hook 'help-mode-hook 'my:help-mode-setup)
    )
  )


(progn ; key-binding
  (defvar my:emacs-home-directory (current-directory))
  (global-set-key (kbd "C-c x") (lambda () (interactive)
                                  (let ((file (concat my:emacs-home-directory "init.el")))
                                    (find-file file))
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
    (define-key ctrl-j-map "c" (lambda () (interactive) (switch-to-buffer (get-buffer-create "*scratch*"))))
    (define-key ctrl-j-map "n" 'my:tab-line-next-tab)
    (define-key ctrl-j-map (kbd "C-n") 'my:tab-line-next-tab)
    (define-key ctrl-j-map "p" 'my:tab-line-prev-tab)
    (define-key ctrl-j-map (kbd "C-p") 'my:tab-line-prev-tab)

    (define-key ctrl-j-map (kbd "C-j") 'dabbrev-expand)

    (define-key ctrl-j-map (kbd "S") 'shell)

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
    (find-file memo-file))
  )


(defun my:after-initialize--windows ()
  ;; remember
  (eval-after-load 'remember (setq remember-data-file "~/memo/notes"))

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
    (global-set-key (kbd "<henkan>")  'newline)

    )
  ;; open memo*.txt
  (let* ((cmd "ls -t ~/memo/memo*.md | head -n 1")
         (memo-file (replace-regexp-in-string "\n" ""  (shell-command-to-string cmd))))
    (and (file-exists-p memo-file) (find-file memo-file)))
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
   (global-set-key (kbd "s-P") 'execute-extended-command) ;; for vscode user
   (my:after-initialize--mac))
  ('gnu/linux ; wsl
   (my:after-initialize--windows))
  (typ (message "the after initialize setting is not found in system-type='%S" typ))
  )

(setq debug-on-error nil)  ;; disable in daily life


;;----------------------------------------
;; init after
;;----------------------------------------
(defvar my:use-after-init nil "これが有効なときにはサードパーティのコードを有効にする。通常はcustom-settings.elで設定される")
(when my:use-after-init
  (load-file (concat (current-directory) "after-init.el")))
