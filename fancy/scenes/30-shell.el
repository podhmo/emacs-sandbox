;;; 30-shell -- *shell* with colored ls output
;;; comint spawns the shell with TERM=dumb; pass TERM inline so GNU ls colorizes.
(let ((process-environment (cons "GIT_PAGER=cat" (cons "PAGER=cat" process-environment))))
  (let ((buf (shell "*shell*")))
    (with-current-buffer buf
      (comint-send-string
       (get-buffer-process buf)
       "TERM=xterm-256color ls -la --color=always && git -C .. log --oneline -5; echo '--- done ---'\n")
      (sleep-for 1.5)
      (fancy:shoot (getenv "FANCY_OUT")))))
