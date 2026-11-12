# tools

init directory (rebirth / fancy など) をまたいで使う補助スクリプト。

| file | 内容 |
|---|---|
| `mac-shot <pid> <out.png>` | macOS: PID の GUI 窓を `screencapture -l` で撮る。exit 3 = 画面収録権限なし |
| `winid.swift <pid>` | macOS: PID の CGWindowID を出力（NS emacs の frame parameter からは取れない） |

画面収録の権限は「大元の起動アプリ」単位（iTerm2、あるいは herdr などの常駐プロセス）。
`swift -e 'import CoreGraphics; print(CGPreflightScreenCaptureAccess())'` で確認できる。
