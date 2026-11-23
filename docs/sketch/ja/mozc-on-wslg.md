# WSLg + Emacs の日本語入力まわりの知見

このリポジトリの mozc 関連設定（`mozc-only/`、`rebirth/init.el` の `my:setup-mozc`）で得た知見のまとめ。

## 環境

- WSL2 + WSLg (Ubuntu)、Nix でビルドした GTK3 Emacs (`withPgtk = false`) = X11 (XWayland) クライアント
- 入力は mozc.el 経由: `emacs -> mozc_emacs_helper -> mozc_server`（fcitx5 は介さない）
- `mozc_emacs_helper` は `emacs-mozc-bin`、`mozc.el` は `emacs-mozc` の deb から取り出し（`tools/mozc-el-setup`）。`emacs-mozc` 本体は apt 版 emacs を依存で引き込むので入れない

## 変換候補を posframe で表示する

`mozc-candidate-style` は差し替え可能。既定の `echo-area`/`overlay` より posframe（カーソル位置の子フレーム）が見やすい。

- `~/.local/share/emacs/site-lisp/mozc/` に `posframe.el`（MELPA、単一ファイル）と `mozc-posframe.el`（[derui/mozc-posframe](https://github.com/derui/mozc-posframe)）を置く
- Mozc 本家の `mozc-cand-posframe.el` は現行リポジトリから削除済みなのでフォークを使う
- 必須なのは以下の手順。`mozc-posframe-initialize` が `posframe-workable-p`（GUI のときのみ真）を見て dispatch table に `'posframe` を登録する

```elisp
(when (and (display-graphic-p)
           (locate-library "posframe")
           (require 'mozc-posframe nil t))
  (mozc-posframe-initialize)
  (setq mozc-candidate-style 'posframe))
```

CUI やファイル不在時は既定の echo-area にフォールバックする。

## 半角/全角キーが連打される問題

### 症状

GUI emacs 上で半角/全角キーを1回押すと数十回分の入力が発生する。

### 原因

WSLg のバグ（[microsoft/WSLG#1457](https://github.com/microsoft/WSLG/issues/1457)）。半角/全角キー（evdev `KEY_GRAVE` → X11 keycode 49 = `Zenkaku_Hankaku`）の **release イベントが届かない** ため、X サーバーが「押しっぱなし」と判断してオートリピートが暴走する。

### 対策

キーを完全に潰す（`.profile` に DISPLAY ガード付きで記述済み）:

```sh
xset -r 49                              # keycode 49 のオートリピート停止
xmodmap -e "keycode 49 = NoSymbol"      # keysym 自体を剥奪（イベントが届かない）
```

注意点:

- X サーバー（XWayland）再起動で消えるので `.profile` 等で毎回適用する必要がある
- keycode 49 は物理的に 半角/全角 の位置。US 配列に切り替えた場合、その位置のキー（`` ` ``）も無効になる
- Wayland ネイティブ（pgtk ビルド等）の場合は別対策が必要（クライアント側リピートなので xkb の `repeat = No` など）

## mozc-mode と input-method の desync

### 症状

`toggle-input-method`（`C-\`）を繰り返すと、モードラインに `[Mozc]` が出ていない（`current-input-method` は nil）のに、キー入力がすべて Mozc に吸われて全角入力になる。しかも `M-x`、`C-x C-c` の confirm の `y` すら mozc-mode-map に奪われるため **Emacs 内からは回復不能**。

### 原因

mozc.el は2層構造:

- `current-input-method`（LEIM 側）— モードラインの `[Mozc]` 表示はこちら
- `mozc-mode`（バッファローカル minor mode）— キーを奪って helper と通信する本体

`C-\` は LEIM 経由で `deactivate-current-input-method-function` → `(mozc-mode nil)` を呼ぶ設計だが、この関数が呼ばれない経路（minibuffer 等の別バッファでの `deactivate-input-method`、トグル中のエラー）があると **mozc-mode だけ ON が残る** desync が起きる。`mozc-mode-string` の既定は nil なので、mozc-mode が残っても表示上は何も分からない。

### 予防（init.el に導入済み）

input-method が OFF になるどの経路でも mozc-mode を確実に OFF にする:

```elisp
(advice-add 'deactivate-input-method :after
            (lambda () (when (bound-and-true-p mozc-mode) (mozc-mode nil))))
```

### 復帰（desync にハマったとき）

キー入力は全部吸われるので **Emacs の外から** `emacsclient` でリセットする:

```sh
emacsclient --eval '(my:mozc-reset)'
```

`my:mozc-reset`（両 init.el に定義済み）は全バッファを走査して `mozc-mode` を強制 OFF にし、session を掃除して helper プロセスを殺す（helper は次回入力時に自動再起動）。

```elisp
(defun my:mozc-reset ()
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
```

前提として emacs server が立っている必要がある（rebirth は既定 `server` で起動済み、mozc-only は `mozc-<pid>`）。シェルエイリアスにしておくと安心:

```sh
alias mozc-reset="emacsclient --eval '(my:mozc-reset)'"
```

サーバーごと吹き飛ばす荒療治は `pkill mozc_emacs_helper` / `pkill mozc_server`（自動再起動する）だが、これだけでは `mozc-mode` は残るので入力は戻らない。必ず `my:mozc-reset` 相当を呼ぶこと。

## 別経路: XIM (fcitx5) 経由

mozc.el を使わず fcitx5 経由で入力する方法もある。環境変数だけでよく、fcitx5 自身の候補 UI が出る。

```sh
fcitx5 -d --disable=wayland   # WSLg では --disable=wayland が必須
XMODIFIERS=@im=fcitx GTK_IM_MODULE=fcitx emacs ...
# C-Space (fcitx5 トリガー) で mozc 活性化
```

- WSLg の weston では fcitx5 の wayland アドオンが input-method-v1 の bind に失敗するため `--disable=wayland` が必須（`.profile` にも同じ起動処理あり）
- XIM フィルタは emacs のキーバインド解決より前段で動くので、`C-Space` は set-mark ではなく fcitx5 トリガー、変換中の `C-g` も mozc の Cancel として届く
- この経路では上記 desync 問題は起きない（mozc.el を使わないため）
