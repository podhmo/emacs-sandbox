# tools

init directory (rebirth / fancy など) をまたいで使う補助スクリプト。

| file | 内容 |
|---|---|
| `mac-shot <pid> <out.png>` | macOS: PID の GUI 窓を `screencapture -l` で撮る。exit 3 = 画面収録権限なし |
| `winid.swift <pid>` | macOS: PID の CGWindowID を出力（NS emacs の frame parameter からは取れない） |
| `x11-shot <pid> <out.png>` | Linux(X11/XWayland): PID の最大ウィンドウを `maim -i` で撮る。要 xdotool, maim。wayland ネイティブ窓は撮れない |
| `mozc-el-setup` | Linux(Debian/Ubuntu): `emacs-mozc` の deb から mozc.el だけを取り出して `~/.local/share/emacs/site-lisp/mozc` に置く。`mozc_emacs_helper` (`emacs-mozc-bin`) は別途 apt で |

画面収録の権限は「大元の起動アプリ」単位（iTerm2、あるいは herdr などの常駐プロセス）。
`swift -e 'import CoreGraphics; print(CGPreflightScreenCaptureAccess())'` で確認できる。
