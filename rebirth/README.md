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
