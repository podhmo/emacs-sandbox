# mozc-only

mozc.el による日本語入力だけを試すための最小 init ディレクトリ。
rebirth の mozc 設定と同じ内容を、`emacs --init-dir` で単体起動できる。

## 使い方

```sh
emacs --init-dir=<repo>/mozc-only /tmp/test.txt &
# C-\ (toggle-input-method) で japanese-mozc を有効化 → 読み入力 → Enter 確定
```

モードラインに `[Mozc]` と出れば有効になっている。

## 必要なもの (WSL/Ubuntu)

- `mozc_emacs_helper` — `sudo apt install emacs-mozc-bin`
- `mozc.el` — `emacs-mozc` の deb から取り出して `~/.local/share/emacs/site-lisp/mozc` に置く
- `mozc_server` — `fcitx5-mozc` 等を入れてあれば接続時に自動起動する

`emacs-mozc` パッケージ自体は依存で apt 版 emacs 本体を引き込むので入れない。
上記の取り出し作業は `tools/mozc-el-setup` が行う。

## 候補表示を posframe にする

GUI 起動時は変換候補がカーソル位置の posframe (子フレーム) に表示される。
以下を同じ site-lisp ディレクトリに置けば自動で有効になる:

- `posframe.el` — MELPA の posframe (単一ファイル。elpa ディレクトリからコピー可)
- `mozc-posframe.el` — https://github.com/derui/mozc-posframe
  (Mozc 本家の `mozc-cand-posframe.el` は現行リポジトリから削除済みのためフォークを使用)

`mozc-posframe-initialize` は `posframe-workable-p` を見るため、
CUI 起動や posframe 不在時は既定の echo-area 表示にフォールバックする。

## 別経路: XIM 経由

mozc.el を使わず、fcitx5 経由で入力する方法。環境変数だけでよい (init 側の設定は不要):

```sh
fcitx5 -d --disable=wayland   # WSLg では --disable=wayland が必須
XMODIFIERS=@im=fcitx GTK_IM_MODULE=fcitx emacs --init-dir=<repo>/mozc-only /tmp/test.txt &
# C-Space (fcitx5 トリガー) で mozc 活性化
```

XIM フィルタは emacs のキーバインド解決より前段で動くため、`C-Space` は
set-mark ではなく fcitx5 のトリガーとして、変換中の `C-g` も keyboard-quit
ではなく mozc の Cancel として届く。

## 検証

init.el は `mozc-<pid>` という名前で server を立てるので、emacsclient から
バッファ内容を読める:

```sh
emacs --init-dir=<repo>/mozc-only /tmp/test.txt &
wid=$(xdotool search --sync --name test.txt | tail -1)
pid=$(xdotool getwindowpid "$wid")     # $! ではなく窓の pid (nix ラッパー対策)
xdotool windowactivate --sync "$wid"
xdotool key ctrl+backslash        # C-\ で japanese-mozc 有効化
xdotool type --delay 60 aiueo
xdotool key Return
emacsclient -s "mozc-$pid" --eval '(with-current-buffer "test.txt" (buffer-string))'
# => "あいうえお"
```

見た目の確認は `tools/x11-shot` (X11 クライアント限定):

```sh
tools/x11-shot "$pid" /tmp/emacs.png
```

## 参考

- <https://github.com/podhmo/im-sandbox> — mozc のカスタムキーマップと
  WSLg での fcitx5 セットアップ、E2E テストの記録 (`docs/sketch/ja/mozc-emacs.md`)
