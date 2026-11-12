# fancy

WSLg / macOS 上の GUI Emacs を「できるだけキレイに」を目指した init directory。
`rebirth` とは独立しており、日々の環境を壊さずに見た目の実験ができる。

## 起動

### そのまま（非 nix）

```console
$ emacs --init-dir=fancy
```

初回起動時に MELPA 等から `fancy/elpa/` にパッケージが自動インストールされる。

### nix（再現可能な環境）

```console
$ nix build .#fancy      # result/bin/emacs-fancy + result/bin/fancy-shot
$ nix run .#fancy        # GUI Emacs 起動
```

- Emacs 31.1 (Linux: GTK3 / macOS: NS) + 依存 elisp パッケージ + フォント（JetBrains Mono / Noto CJK / Nerd Font Symbols）をすべて nix 側で解決
- macOS は fontconfig を使わない（CoreText）ので、初回だけフォントを入れる。CJK は標準の Hiragino Sans を使う

  ```console
  $ result/bin/fancy-install-fonts               # ~/Library/Fonts/emacs-fancy/ に JetBrains Mono + Symbols Nerd Font をコピー
  $ result/bin/fancy-install-fonts --uninstall   # 削除
  ```
- `FANCY_NIX` が立つと `package-install` をスキップし、書き込み先を `$XDG_DATA_HOME/emacs-fancy` / `$XDG_CACHE_HOME/emacs-fancy` に逃がす（nix store は read-only）

## スクリーンショット

```console
$ fancy/bin/shot <scene.el> <out.png> [timeout]
$ result/bin/fancy-shot <scene.el> <out.png>   # nix 版
```

`scenes/` にシーンを置くと、その状態を撮影できる。生成物は `shots/`（gitignore）に出す運用。

| scene | 内容 |
|---|---|
| `00-scratch` | 起動直後のベースライン |
| `05-visuals` | ligature / indent-bars / page-break-lines / rainbow-delimiters |
| `10-markdown` | mixed-pitch + 見出しスケーリング |
| `15-dashboard` | 起動ダッシュボード |
| `20-minibuffer` | vertico-posframe（M-x 風ポップアップ） |
| `25-whichkey` | which-key-posframe（`C-x` prefix） |
| `30-shell` | comint + ANSI 色 + git log |
| `35-corfu` | corfu + kind-icon 補完ポップアップ |
| `40-dired` | nerd-icons-dired |
| `45-zoom` | zoom（黄金比ウィンドウ配分） |

### 成否判定の設計

プロセスの終了コードではなく「`out.png` が非空で生成されたか」で判定する。
Emacs は初期化エラーで debugger が開き入力待ちになることがあるので、
`etc/shots.el` 側で `debug-on-error` を切り、`kill-emacs-hook` で
`*Messages*` / `*Warnings*` / `*Compile-Log*` / `*Backtrace*` を `out.log` にダンプする。
タイムアウト時は `out.failed.png`（Linux: scrot で全画面 / macOS: `tools/mac-shot` で Emacs の窓）を撮ってから終了。

## WSLg 特有の知見

- X の root window は黒い（各ウィンドウが RDP 経由で個別転送される）ので全画面撮影は不可
  - フレーム単位 → `x-export-frames`（クリーン）
  - child frame（posframe）込み → `xwd -id <outer-window-id>` + ImageMagick
- comint は子プロセスに `TERM=dumb` を強制する → `TERM=xterm-256color ls --color=always` のようにコマンド内で指定
- `emacs -l scene.el` で開いた minibuffer は `post-command-hook` が走らず vertico が候補を計算しない → `(run-hooks 'post-command-hook)` を手動発火
- solaire-mode の暗転時に行番号 gutter だけ明るい帯が残る → line-number をファイルバッファのみに限定

## macOS 特有の知見

- NS ビルドには `x-export-frames` が無い → `screencapture -R` でフレームの矩形（`frame-edges` の outer-edges、座標系は一致）を撮る。posframe（子フレーム = 別 NSWindow）も写る。`frame` / `window` の区別は無視
- 「画面収録」権限が必要。無いと壁紙しか写らないので、`bin/shot` は最初に権限を確認して exit 3 で止まる（詳細は [`tools/README.md`](../tools/README.md)）
- フォントは CoreText 経由。nix の `XDG_DATA_DIRS` は効かない → `fancy-install-fonts`
  - Symbols Nerd Font が無いと nerd-icons の Private Use Area を Hiragino が拾い、アイコンが漢字に化ける。PUA を明示的に Nerd Font に割り当てている
- `ns-transparent-titlebar` + `ns-appearance dark` でタイトルバーをテーマに馴染ませる
- 30-shell は rc を読まない bash を samples/ で起動する（ユーザーの shell rc だとホスト名やホームの一覧が写り込む）

## nix 特有の知見

- flake の source は **git 追跡ファイルのみ** — 新規シーンは `git add` しないと store に入らない
- `vertico-posframe` は `ep.elpaPackages`（GNU ELPA）側にある
- `svg-lib` のキャッシュは `svg-lib-icons-dir` を fancy の data dir に逃がす必要がある

## パッケージ

- テーマ・基盤: `doom-themes`（doom-one）, `solaire-mode`, `spacious-padding`, `posframe`
- 補完: `vertico`, `vertico-posframe`, `marginalia`, `orderless`, `corfu`, `kind-icon`
- アイコン: `nerd-icons`, `nerd-icons-completion`, `nerd-icons-dired`, `svg-lib`
- 表示: `doom-modeline`, `minions`, `hl-todo`, `rainbow-delimiters`, `indent-bars`, `page-break-lines`, `diff-hl`, `ligature`
- 文書・操作: `markdown-mode`, `mixed-pitch`, `which-key`, `which-key-posframe`, `dashboard`, `zoom`

## ファイル構成

```
fancy/
  init.el          # メイン設定
  early-init.el    # 起動抑制・フレーム既定・eln-cache リダイレクト
  etc/shots.el     # 撮影機構（frame / window メソッド、診断ダンプ）
  bin/shot         # シーン実行ラッパー
  scenes/*.el      # 撮影シーン
  samples/         # 撮影用サンプルファイル
  shots/           # 生成物置き場（gitignore）
  elpa/            # 非 nix 時のパッケージ（gitignore）
```
