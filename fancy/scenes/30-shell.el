;;; -*- lexical-binding: t -*-
;;; 30-shell -- *shell* with colored ls output
;;; comint spawns the shell with TERM=dumb; pass TERM inline so GNU ls colorizes.
;;; Run a bare bash in samples/: the user's own shell rc would leak the hostname
;;; (prompt) and whatever directory it starts in into a shot meant for sharing.
(require 'shell) ; defvar explicit-* first, or the let below binds them lexically
(let* ((default-directory (file-name-as-directory (expand-file-name "samples" fancy:dir)))
       (explicit-shell-file-name "bash")
       (explicit-bash-args '("--noprofile" "--norc" "-i"))
       (process-environment
        (append '("GIT_PAGER=cat" "PAGER=cat" "PS1=\\W $ ") process-environment)))
  (let ((buf (shell "*shell*")))
    (with-current-buffer buf
      (comint-send-string
       (get-buffer-process buf)
       ;; short listing: no owner column (= local user name) in the shot.
       ;; BSD ls (macOS) has no --color=always; fall back to -G
       "{ TERM=xterm-256color ls -A --color=always 2>/dev/null || CLICOLOR_FORCE=1 ls -AG; } && { git -C .. log --oneline -5 2>/dev/null || echo '(not a git checkout)'; }; echo '--- done ---'\n")
      (sleep-for 1.5)
      (fancy:shoot (getenv "FANCY_OUT")))))
