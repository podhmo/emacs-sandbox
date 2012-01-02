;; 行数表示
(line-number-mode t)

;; スタートアップページを表示しない
(setq inhibit-startup-message t)

;; バックアップファイルを作らない
(setq backup-inhibited t)

;; Macのキーバインドを使う。optionをメタキーにする。
(when (boundp 'mac-key-mode)
  (mac-key-mode 1)
  (setq mac-option-modifier 'meta))

;; タブキー
(setq default-tab-width 4)
(setq indent-line-function 'indent-relative-maybe)
(setq-default indent-tabs-mode nil)

;; シフト + 矢印で範囲選択
(setq pc-select-selection-keys-only t)
(pc-selection-mode 1)

;; メニューバーを隠す
(tool-bar-mode -1)

;;; emacsclient サーバを起動
(when (and (require 'server nil t) (not (server-running-p)))
  (server-start))

;;; 圧縮されたファイルも編集できるようにする
(auto-compression-mode t)

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;;; 対応する括弧を光らせる。
(show-paren-mode 1)

;;; ホイールマウス
(mouse-wheel-mode t)
(setq mouse-wheel-follow-mouse t)

;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;;; clipboard共有
(setq x-select-enable-clipboard t)

