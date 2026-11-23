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
  ;; 候補表示: 'echo-area (既定) / 'overlay / 'popup / 'posframe
  ;; posframe は derui/mozc-posframe (site-lisp に配置) + posframe.el が必要。
  ;; GUI 専用なので display-graphic-p を確認してから有効化する。
  ;; (setq mozc-candidate-style 'overlay)
  (when (and (display-graphic-p)
             (locate-library "posframe")
             (require 'mozc-posframe nil t))
    (mozc-posframe-initialize) ; dispatch-table に 'posframe を登録 (GUI のみ)
    (setq mozc-candidate-style 'posframe))
  ;; minibuffer では input method を無効にしておく
  (add-hook 'minibuffer-setup-hook 'deactivate-input-method)
  ;; desync 対策:
  ;; mode-line の [Mozc] は current-input-method 側の表示。トグルの取りこぼし等で
  ;; mozc-mode だけが ON に残ると「見た目は OFF なのに全角入力される」状態になる。
  ;; input-method が OFF になったら mozc-mode も確実に OFF にする。
  (advice-add 'deactivate-input-method :after
              (lambda () (when (bound-and-true-p mozc-mode) (mozc-mode nil)))))

(defun my:mozc-reset ()
  "mozc の desync から復帰する緊急リセット。
mozc-mode はバッファローカルなので、全バッファを走査して強制 OFF にする。
session を掃除し、helper プロセスも殺す (次回入力時に自動再起動される)。

desync 中は M-x すら mozc に吸われて入力できないので、外部からは
  emacsclient -s <server-name> --eval '(my:mozc-reset)'
として呼ぶ (server-name は init.el 末尾の mozc-<pid>)。"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p mozc-mode) (mozc-abort))
      (when (fboundp 'mozc-clean-up-changes-on-buffer)
        (ignore-errors (mozc-clean-up-changes-on-buffer)))))
  (when (boundp 'mozc-helper-process)
    (let ((proc mozc-helper-process))
      (when (processp proc) (kill-process proc))))
  (message "mozc reset"))

;; 検証用: emacsclient からバッファ内容を読めるように server を立てる。
;; 日常の server と衝突しないよう pid 付きの名前にする
;;   emacsclient -s mozc-<pid> --eval '(buffer-string)'
(require 'server)
(setq server-name (format "mozc-%d" (emacs-pid)))
(unless (server-running-p) (server-start))

;;; init.el ends here
