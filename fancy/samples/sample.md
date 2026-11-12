# Fancy Emacs 見た目改善サンプル

WSLg 上の Emacs を **mac 風にきれいに** するための検証用ドキュメント。
日本語と English を *混在* させて表示確認する。

## 特徴

- JetBrains Mono で等幅表示
- `doom-one` テーマ + `doom-modeline`
- 余白 (internal border / fringe) たっぷり

### コードブロック

```elisp
(defun fancy:shoot (file &optional method)
  "Save a screenshot to FILE."
  (redisplay t)
  (sit-for 0.3)
  (let ((data (x-export-frames nil 'png)))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert data))))
```

```python
def greet(name: str) -> str:
    # TODO: i18n
    return f"hello, {name}"
```

## 表

| name        | role        | status |
|-------------|-------------|--------|
| vertico     | minibuffer  | ok     |
| doom-one    | theme       | ok     |
| posframe    | popup       | ok     |

> 引用文: 「見た目が良いと気分が上がる」
> -- 誰か

1. first item
2. second item
   - nested
   - nested again

[link to github](https://github.com/podhmo/emacs-sandbox)

- [x] 済みタスク
- [ ] 未タスク
