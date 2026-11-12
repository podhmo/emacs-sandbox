;;; 10-markdown -- markdown-mode rendering
(find-file (expand-file-name "samples/sample.md" fancy:dir))
(goto-char (point-min))
(fancy:shoot (getenv "FANCY_OUT"))
