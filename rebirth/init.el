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

(use-package macros :load-path my:local-lisp-dir
  :demand t; 即時ロードしたい
  )
(use-package utils :load-path my:local-lisp-dir
  :demand t; 即時ロードしたい
  )



(use-package emacs ; core settings
  :custom
  ;; --- UI・応答設定 ---
  ;; yes/no を y/n にし、終了時に確認する
  (use-short-answers t)
  (confirm-kill-emacs 'y-or-n-p)
  ;; ビープ音を視覚的な通知（フラッシュ）に変更 [1]
  (visible-bell t)
  ;; キー入力の表示速度、ミニウィンドウの挙動
  (echo-keystrokes 0.2)
  (resize-mini-windows t)

  ;; --- ファイル・補完設定 ---
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)

  ;; --- バックアップ・保存設定 ---
  (delete-by-moving-to-trash t)    ; ゴミ箱へ移動
  (make-backup-files nil)          ; バックアップファイルを作らない
  (auto-save-default nil)          ; 自動保存をデフォルトでは無効
  (create-lockfiles nil)           ; ロックファイルを作らない
  (backup-directory-alist '((".*" . "~/.emacs.d/backup")))

  ;; --- 検索・ハイライト設定 ---
  (search-highlight t)
  (query-replace-highlight t)

  :init
  ;; バッファローカルな変数のデフォルト値設定（ロード前に行う）
  (setq-default indent-tabs-mode nil
                tab-width 4
                fill-column 120)

  :config
  ;; --- モードの有効化 ---
  ;; 外部でのファイル変更を自動反映 [3]
  (global-auto-revert-mode t)
  ;; 対応する括弧を表示
  (show-paren-mode 1)
  ;; 選択範囲（マーク）を視覚化
  (transient-mark-mode t)
  ;; 画像ファイルを自動表示
  (auto-image-file-mode t)

  ;; --- Emacs Serverの設定 ---
  (condition-case err
      (progn
        (require 'server)
        (unless (server-running-p)
          (server-start)))
    (error (message "emacsclient load fail"))))



(use-package emacs ; auto-save
  :custom
  (auto-save-visited-interval 0.5)

  :preface
  (defvar my:disable-auto-save-visited-mode-alist nil)

  (defun my:find-file-hook--disable-auto-save ()
    (require 'cl-lib)

    (let ((fname (buffer-file-name)))
      (cl-dolist (arg my:disable-auto-save-visited-mode-alist)
        (cl-destructuring-bind (fn . x) arg
          (when (funcall fn x fname)
            (message "# auto-save-visited-mode is disabled by %s %s" fn x)
            (setq-local auto-save-visited-mode nil)
            (cl-return nil))))))

  :init
  (push `(string-prefix-p . "/tmp") my:disable-auto-save-visited-mode-alist)
  :hook
  (find-file . my:find-file-hook--disable-auto-save)
  :config
  (auto-save-visited-mode t)
  )



(use-package recentf ;; 最近使ったファイル
  :custom
  (recentf-max-saved-items 100)
  (recentf-exclude '("/tmp/" "/ssh:"))
  :config
  (recentf-mode 1))



(use-package elisp-mode
  :preface
  ;; 自作関数はバイトコンパイラの警告を避けるために :preface に記述
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
  :bind (:map emacs-lisp-mode-map
              ("C-c C-e" . eval-defun)
              ("C-c C-l" . eval-buffer)
              ("C-x C-s" . my:elisp-pretty-print-region))
  :init
  ;; 起動時に反映させたい変数の設定
  (setq initial-major-mode 'emacs-lisp-mode))



(use-package make-mode :defer t
  :config
  ;; auto-save中で定期的にy-or-n-pで尋ねてくるのはうるさすぎるので *Messages* に書くだけにする。元の関数を上書きしている。
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

  )



;; javascript
(use-package js :defer t
  :mode ("\\.mjs\\'" . js-mode)
  :preface
  ;; バイトコンパイル時の警告を防ぐため、自作関数は :preface に定義
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
        (setq-local line-move-visual nil)))) ; C-nでの移動は論理行ではなく物理行にする
  :hook
  ;; -hook サフィックスは自動補完されるため省略可能
  (js-mode . electric-pair-mode)
  (js-json-mode . my:open-with-low-cost-mode--if-huge-first-line))



(use-package shell :defer t
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :init
  ;; M-x で *shell* bufferを開くときにはother windowに開く
  (add-to-list 'display-buffer-alist
               '("\\*shell\\*"
                 (display-buffer-at-bottom)
                 (reusable-window . t))))



(use-package text-mode
  :init
  ;; markdown-modeがない場合はtext-modeに
  (unless (locate-library "markdown-mode")
    (add-to-list  'auto-mode-alist '("\\.md" .  text-mode))
    )

  :preface
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

  :bind (:map text-mode-map
              ("C-M-i" . dabbrev-expand)
              ("<tab>" . my:indent-rigitly)
              ("<backtab>" . my:unindent-rigitly)
              ("<S-iso-lefttab>" . my:unindent-rigitly)) ;; for skk
  :hook (text-mode . (lambda ()
                       (setq-local line-spacing 0.05) ;; ちょっと行間を拡げる
                       ))
  )



(use-package interactives :load-path my:local-lisp-dir
  :demand t; 即時ロードしたい
  )



;; タブ移動
(use-package tab-line
  :demand t  ; global-tab-line-mode を即座に有効にするため即時ロード
  :preface
  (defun my:tab-line-tabs-window-buffers ()
    "fileを持つものだけを対象にしたbufferを集める"
    (sort
     (seq-remove (lambda (b) (null (buffer-file-name b)))
                 (buffer-list))
     (lambda (x y) (string< (buffer-name x) (buffer-name y)))))

  (defun my:tab-line-next-tab ()
    "tab-lineの表示にしたがった場合の次のタブに移動"
    (interactive)
    (require 'cl-lib)
    (funcall my:tab-line-switch-to-buffer-function
             (buffer-name
              (cl-block :b
                (let ((current-buf (current-buffer))
                      (buffers (funcall tab-line-tabs-function))
                      (foundp nil))
                  (cl-dolist (b buffers)
                    (when foundp
                      (cl-return-from :b b))
                    (when (equal current-buf b)
                      (setq foundp t)))
                  (cl-return-from :b (car buffers)))))))

  (defun my:tab-line-prev-tab ()
    "tab-lineの表示にしたがった場合の前のタブに移動"
    (interactive)
    (require 'cl-lib)
    (funcall my:tab-line-switch-to-buffer-function
             (buffer-name
              (cl-block :b
                (let ((current-buf (current-buffer))
                      (buffers (funcall tab-line-tabs-function))
                      (prev-buf nil))
                  (cl-dolist (b buffers)
                    (when (equal current-buf b)
                      (cl-return-from :b
                        (or prev-buf (car (last buffers)))))
                    (setq prev-buf b)))))))

  :bind (("C-c <muhenkan>" . my:tab-line-prev-tab)
         ("C-c C-p"         . my:tab-line-prev-tab)
         ("C-c <henkan>"   . my:tab-line-next-tab)
         ("C-c C-n"         . my:tab-line-next-tab))

  :config
  (global-tab-line-mode t)
  (setq tab-line-tabs-function 'my:tab-line-tabs-window-buffers)
  (setq my:tab-line-switch-to-buffer-function 'switch-to-buffer))



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

  (progn ;; windowsのWSL上で半角/全角キーを入力すると連続してタイプされるの抑制したい
    (global-set-key (kbd "<zenkaku-hankaku>") 'ignore)
    (global-set-key (kbd "C-<zenkaku-hankaku>") 'ignore)
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

  ;; open memo*.md
  (let* ((cmd "ls -t ~/memo/memo*.md | head -n 1")
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
