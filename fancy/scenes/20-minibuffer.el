;;; -*- lexical-binding: t -*-
;;; 20-minibuffer -- M-x with vertico-posframe + marginalia (needs 'window capture)
;;; NOTE: command-line -l/--eval runs outside the real command loop, so
;;; post-command-hook (vertico--exhibit) never fires on its own. Insert input
;;; then fire the hook manually to populate candidates before shooting.
(fancy:frame-center)
(run-at-time 10 nil #'kill-emacs) ; backstop: never leave emacs running
(run-at-time
 1.0 nil
 (lambda ()
   (insert "find-f")
   (run-hooks 'post-command-hook)
   (run-at-time
    0.8 nil
    (lambda ()
      (run-hooks 'post-command-hook)
      (fancy:shoot (getenv "FANCY_OUT") 'window)
      (abort-recursive-edit)))))
(execute-extended-command nil)
