;;; -*- coding: utf-8; lexical-binding: t -*-
;;; init.el -- mozc 入力だけを試すための最小 init ディレクトリ
;;
;; usage:
;;   emacs --init-dir=<this dir> /tmp/test.txt
;;   # C-\ (toggle-input-method) で japanese-mozc を有効化
;;
;; 経路: emacs -> mozc_emacs_helper -> mozc_server  (fcitx5 は介さない)
;; 必要なもののインストールは tools/mozc-el-setup を参照。
;;
;; XIM 経由 (fcitx5 経由) を試す場合は環境変数だけでよい:
;;   XMODIFIERS=@im=fcitx GTK_IM_MODULE=fcitx emacs --init-dir=<this dir> ...
;;   → C-Space (fcitx5 トリガー) で mozc 活性化。この場合以下の mozc.el は不要。

;; --- mozc.el ---
;; emacs-mozc パッケージは apt 版 emacs を依存で引き込むため使わず、
;; emacs-mozc-bin (helper のみ) + deb から取り出した mozc.el の組み合わせ。
(dolist (dir '("~/.local/share/emacs/site-lisp/mozc"
               "/usr/share/emacs/site-lisp/mozc"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))
(when (locate-library "mozc")
  (require 'mozc)
  (setq default-input-method "japanese-mozc")
  ;; 候補表示: 'echo-area (既定) / 'overlay / 'popup
  ;; (setq mozc-candidate-style 'overlay)
  ;; minibuffer では input method を無効にしておく
  (add-hook 'minibuffer-setup-hook 'deactivate-input-method))

;; 検証用: emacsclient からバッファ内容を読めるように server を立てる。
;; 日常の server と衝突しないよう pid 付きの名前にする
;;   emacsclient -s mozc-<pid> --eval '(buffer-string)'
(require 'server)
(setq server-name (format "mozc-%d" (emacs-pid)))
(unless (server-running-p) (server-start))

;;; init.el ends here
