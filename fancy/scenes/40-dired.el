;;; 40-dired -- dired with nerd icons
(dired (expand-file-name "samples" fancy:dir))
(revert-buffer)
(fancy:shoot (getenv "FANCY_OUT"))
