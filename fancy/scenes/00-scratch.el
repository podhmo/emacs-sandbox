;;; 00-scratch -- baseline: *scratch* + elisp code
(find-file (expand-file-name "etc/shots.el" fancy:dir))
(goto-char (point-min))
(fancy:shoot (getenv "FANCY_OUT"))
