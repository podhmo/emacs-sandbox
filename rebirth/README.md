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

future work (TODO)

- (use-packageをまじめに使う)
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

- tab-barで常に新しいタブで開きたい。" *"から始まるバッファはタブの対象外
- skk-modeで無効にした `C-j` の影響でタブ移動が失敗することがある
- kill-ringの選択が手軽にできない
- backupファイルがまだ同一ディレクトリ内に作られてしまう
