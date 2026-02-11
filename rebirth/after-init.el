;;; -*- coding: utf-8; lexical-binding: t -*-

;; ;; external package
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;;
;; (package-install 'writeroom-mode) ; -> zen-mode in vscode
;; (package-install 'ddskk-postframe);  skkの変換候補を良いかんじに表示してくる
;;
;; (load-file "./extra.el")

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
