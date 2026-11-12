# rebirth

## 読み込まれる順序

1. early-init.el
2. init.el
3. after-init.el # ただしcustom-settings.elでmy:use-after-init == tのときだけ

サードパーティパッケージを有効にする

custom-settings.el
```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-

(setq my:use-after-init t)
```


## 動作確認 (macOS)

エージェントからも叩ける GUI 起動チェック。rebirth/ を一時ディレクトリにコピーして起動するので、
日常の `~/.emacs.d`（history / recentf など）は汚さない。

```console
$ rebirth/bin/check [out-dir] [timeout]                       # PATH の emacs
$ EMACS=$(nix build .#default --print-out-paths)/bin/emacs rebirth/bin/check /tmp/rebirth-check-31
```

- 出力: `report.txt`（status / version / theme / font / packages …）, `messages.log`（`*Messages*` `*Warnings*` `*Compile-Log*` `*Backtrace*`）, `shot.png`
- 成否は emacs の終了コードではなく `report.txt` の `status: OK` で判定する
  - `bin/probe.el` をコピー側の `early-init.el` 末尾で読み込み、タイマーでレポートを書く。init.el がエラーで debugger に入っていてもタイマーは動くので必ず NG として記録される
  - probe は `server-name` を差し替えるので、日常の emacs server とは衝突しない
- スクリーンショットは `screencapture -l <CGWindowID>`。NS の frame parameter からは CGWindowID が取れないので `bin/winid.swift` で PID から引く
  - 「画面収録」権限が必要。無いと壁紙しか写らないので skip する。`swift -e 'import CoreGraphics; print(CGPreflightScreenCaptureAccess())'` で確認できる
  - 権限は「大元の起動アプリ」単位。herdr 等の常駐プロセス配下で動かす場合は iTerm2 ではなく herdr のバイナリ (`~/.local/bin/herdr`) を許可する。付与後はアプリの再起動が必要
- init.el は起動時に `~/memo/memo*.md`（私的メモ）を開くので、撮影前に `REBIRTH_CHECK_FILE`（既定: `rebirth/README.md`）だけを表示し、他のファイルバッファは閉じる
- `INPLACE=1` で rebirth/ 自体を使う（probe は init 後に `-l` で読むので init のハングはタイムアウトでしか検知できない）、`KEEP=1` でコピーを残す

## 補足情報

今回からの変更予定

- view onlyで開くのがデフォルトの操作だったのを辞める
- 組み込みの機能しか使わない制限を捨てる
- 自作の機能を作るのを避けていたがこの制限を捨てる
- 日本語入力の開始は常に`C-\` (toggle-input-method) で行う
- tab-barは捨てる。tab-lineの方を使う

future work (TODO)

- (use-packageをまじめに使う。load-fileやprognとコメントでごまかさない)
- (tree-sitterをつかった方のモードをまじめに使う)
- (領域選択をしてLLMに渡す)
- (領域選択をして翻訳する)
- (音声入力してその後編集という感じで作業できるようにする)
- (前回なくしてしまった位置つきのメモ機能を復活させる)
- (前回なくしてしまったプロジェクト移動用の選択UIを復活させる)
- (自己修復的にループをするような仕組みをelisp上に導入したい)

前回からの引き継ぎ

- WSLから利用を主目的にする。日本語の入力はSKKを使う。
- org-modeとか複雑なアプリには手を出さない
- format buffer的なものを機能させる
- `C-j` + 何か で移動させるという機能 (これを手軽に設定しまくれるようにしたい)
- なるべくinit.el一つで済ませる

細々とした不備

- skk-modeで無効にした `C-j` の影響でタブ移動が失敗することがある
- kill-ringの選択が手軽にできない
- backupファイルがまだ同一ディレクトリ内に作られてしまう


---

## これならわかるuse-package

Before: 従来の progn による一括設定

```lisp
(progn ; my-sample-mode の設定
  ;; 1. [init相当] ロード前に必要な変数設定
  (setq my-sample-variable t)

  ;; 2. [mode相当] 拡張子の関連付け
  (add-to-list 'auto-mode-alist '("\\.sample\\'" . my-sample-mode))

  ;; 3. [hook相当] フックの登録
  (add-hook 'my-sample-mode-hook 'my-setup-function)

  ;; 4. [config相当] ロード後に実行したい処理
  (with-eval-after-load 'my-sample
    (my-sample-initialize-api)
    (setq my-sample-active-flag t))
)
```

After: use-package による構造化

use-package 内部では progn を書かなくても、複数の式をキーワードごとにまとめて記述できます。

```lisp
(use-package my-sample
  ;; 2. [mode] auto-mode-alist への追加。同時に遅延ロードも設定される
  :mode "\\.sample\\'"

  ;; 3. [hook] add-hook の代わり。"-hook" サフィックスは自動補完される
  :hook (my-sample-mode . my-setup-function)

  ;; 1. [init] パッケージがロードされる「前」に実行される
  :init
  (setq my-sample-variable t)

  ;; 4. [config] パッケージがロードされた「後」に実行される
  :config
  (my-sample-initialize-api)
  (setq my-sample-active-flag t))
```
