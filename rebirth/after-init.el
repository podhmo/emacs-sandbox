;;; -*- coding: utf-8; lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; (package-install 'ddskk-postframe);  skkの変換候補を良いかんじに表示してくる



(use-package writeroom-mode ; zen-mode in vscode
  :ensure t)



(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown") ; または "pandoc --from=markdown --to=html5"
  :bind (("C-c <henkan>" . my:tab-line-next-tab)
         ("C-c <muhenkan>" . my:tab-line-prev-tab)
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
