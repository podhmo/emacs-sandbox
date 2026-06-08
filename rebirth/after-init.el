;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; (package-install 'ddskk-postframe);  skkの変換候補を良いかんじに表示してくる


;; nixでインストールしたEmacsをFinderから起動した場合にPATHなどが引き継がれないので`executable-find'などが失敗する。 `open -a emacs`などで開けば解決するが不便。
;; 動作確認 ↓ で C-x C-e
;; (getenv "PATH")
;; exec-path
;; (executable-find "ghq")
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  ;; .zprofile や .zshrc から引き継ぎたい環境変数を指定
  ;; NIX_SSL_CERT_FILE なども入れておくと、Emacs 内での HTTPS 通信エラーを防げます
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "NIX_SSL_CERT_FILE"))
  (exec-path-from-shell-initialize)
  )


(use-package writeroom-mode ; zen-mode in vscode
  :ensure t)


(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown") ; または "pandoc --from=markdown --to=html5"

  :bind (:map markdown-mode-map
              ("C-c <henkan>" . my:tab-line-next-tab)
              ("C-c <muhenkan>" . my:tab-line-prev-tab)

              ;; indent/unindentのtoggleだけで十分
              ("TAB" . my:indent-rigitly)
              ("<tab>" . my:indent-rigitly)
              ("S-TAB" . my:unindent-rigitly)
              ("<backtab>" . my:unindent-rigitly)
              ("S-<tab>" . my:unindent-rigitly)
              ("<S-iso-lefttab>" . my:unindent-rigitly) ;; for skk

              ;; 移動はここでも有効になってほしい
              ("C-c <muhenkan>" . my:tab-line-prev-tab)
              ("C-c C-p"         . my:tab-line-prev-tab)
              ("C-c <henkan>"   . my:tab-line-next-tab)
              ("C-c C-n"         . my:tab-line-next-tab)

              ("C-c n" . markdown-forward-block)
              ("C-c p" . markdown-backward-block)

              ("C-c c" . side-pocket:toggle-buffer))

  :config
  ;; よく使うカスタマイズ
  (setq markdown-asymmetric-header t)      ; # 見出しを非対称にする（GitHub風）
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (setq markdown-gfm-uppercase-checkbox t) ; [X] のチェックボックスを大文字に
  ;; 便利なフック
  (add-hook 'markdown-mode-hook #'visual-line-mode))


(use-package go-mode
  :mode (("\\.go$" . go-mode))
  )


(use-package moonbit-mode
  :vc (:url "https://github.com/podhmo/moonbit-mode" :rev "856b781053212235280caef06a9f9abf387bc9b9")
  :ensure t
  :init
  (add-to-list 'treesit-language-source-alist
               '(moonbit "https://github.com/moonbitlang/tree-sitter-moonbit"))

  :hook (moonbit-mode . my:moonbit-setup)
  :bind (:map moonbit-mode-map
              ("C-x C-s" . moonbit-format-buffer))
  :config
  (defun my:moonbit-setup ()
    (flymake-mode t)
    (eldoc-mode t)
    )
  )

;;----------------------------------------
;; TODO: tree-sitter
;;----------------------------------------
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
;; (treesit-install-language-grammar 'markdown-inline)


;; ===============================================
;; 現代的なミニバッファ補完環境（Vertico + Consult + Marginalia + Orderless）
;; ===============================================

(use-package vertico
  :ensure t
  :demand t                          ; 即時ロード（必須）
  :custom
  (vertico-count 15)                 ; 表示する候補数
  (vertico-cycle t)                  ; 最後の候補から最初に戻る（循環）
  (vertico-resize nil)               ; ミニバッファの高さを自動調整しない
  :init
  (vertico-mode 1)

  :bind (:map vertico-map
              ("C-f" . vertico-exit)           ; 確定して退出
              ("RET" . vertico-exit)
              ("TAB" . vertico-insert)))       ; 部分挿入（便利）

;; 補完履歴を保存・ソート（Verticoが履歴順で並べ替えるようになる）
(use-package savehist
  :ensure t
  :init
  (savehist-mode 1))

;; Marginalia：候補の横に便利な注釈（ファイル種別、ドキュメント、キーなど）を表示
(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-align 'right)          ; 注釈を右寄せ（見やすい）
  :init
  (marginalia-mode 1)

  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))   ; 注釈の種類を切り替え

;; Orderless：スペース区切りで「どの順番でも」マッチする強力な補完スタイル
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))          ; orderlessを最優先
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion))           ; ファイル補完は部分一致も併用
     (buffer (styles orderless))))
  ;; 必要に応じて追加（Consult Wiki推奨）
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  )

;; Consult：超高機能な補完コマンド群（find-file、grep、ripgrep、imenu、buffer切り替えなど）
;; TODO: まだ真面目に調整をしていない。
(use-package consult
  :ensure t
  :after (vertico orderless)
  :bind (
         ("C-x b"   . consult-buffer)          ; バッファ切り替え（最近使った順＋プレビュー）
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)  ; プロジェクト内バッファ

         ("M-g g"   . consult-goto-line)       ; 行ジャンプ（プレビュー付き）
         ("M-g M-g" . consult-goto-line)
         ("M-g o"   . consult-outline)         ; アウトライン
         ("M-g i"   . consult-imenu)           ; imenu（関数/変数一覧）
         ("M-g I"   . consult-imenu-multi)
         ("M-g f" . my:consult-git-ls-files)
         ("M-g l" . my:consult-ghq-list)

         ("M-s d"   . consult-find)            ; ファイル検索（fd/find）
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)            ; rg/grep（プロジェクト全体）
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)         ; ripgrep（超高速・おすすめ）
         ("M-s l"   . consult-line)            ; 現在のバッファ内検索（ライブプレビュー）
         ("M-s L"   . consult-line-multi)

         ("M-y"     . consult-yank-pop)        ; kill-ringをプレビュー付きで貼り付け
         ("C-x C-r" . consult-recent-file)     ; 最近開いたファイル

         :map isearch-mode-map
         ("M-s l" . consult-line))             ; isearch中にもconsult-line

  :custom
  (consult-preview-key '(:debounce 0.2 any))  ; プレビューを少し遅延（軽くする）
  (consult-narrow-key "<")                    ; 候補をタイプで絞り込み（例: <b でbufferのみ）
  :config
  ;; 必要に応じて追加設定
  (setq consult-project-root-function #'consult--default-project--root) ; project.el連携

  (defun my:consult-git-ls-files ()
    "プロジェクト内のファイルを `git ls-files` で一覧表示（project-current対応）。
Gitリポジトリでなければメッセージを表示。"
    (interactive)
    (let* ((proj (project-current t))  ; t = プロジェクトがなければプロンプトで選択
           (root (and proj (project-root proj)))
           (default-directory (or root default-directory)))

      (if (not (vc-git-root default-directory))
          (message "Not inside a Git repository. (project root: %s)"
                   (or root default-directory))
        ;; Gitリポジトリ内なら git ls-files を実行
        (let* ((cmd "git ls-files -z --full-name --")
               (cands (split-string (shell-command-to-string cmd) "\0" t)))
          (find-file
           (consult--read cands
                          :prompt "Git Files: "
                          :sort nil               ; git ls-files の順序を維持
                          :require-match t
                          :category 'file
                          :state (consult--file-preview)
                          :history 'file-name-history
                          :add-history (thing-at-point 'filename)))))))

  (defun my:consult-ghq-list ()
    "ghq で管理しているリポジトリ一覧を表示してディレクトリを選択。
選択したら `dired` で開く。"
    (interactive)
    (let* ((cmd "ghq list -p")
           (cands (split-string (shell-command-to-string cmd) "\n" t))
           (default-directory (or (car cands) default-directory)))  ; 念のため

      (if (null cands)
          (message "ghq リポジトリが見つかりませんでした。")
        (let ((selected (consult--read cands
                                       :prompt "ghq Repos: "
                                       :sort nil          ; ghqの出力順を維持
                                       :require-match t
                                       :category 'file
                                       :state (consult--file-preview)
                                       :history 'file-name-history
                                       :add-history (thing-at-point 'filename))))
          (when selected
            (dired selected))))))
  )
