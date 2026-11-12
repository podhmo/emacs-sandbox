;;; -*- lexical-binding: t -*-
;;; 25-whichkey -- which-key-posframe popup over a dimmed frame
;;; simulate typing the C-x prefix; which-key pops up after its idle delay
(setq unread-command-events (listify-key-sequence "\C-x"))
(run-at-time
 (+ which-key-idle-delay 0.8) nil
 (lambda ()
   (fancy:shoot (getenv "FANCY_OUT") 'window)
   (abort-recursive-edit)))
;; backstop in case something hangs
(run-at-time 20 nil #'kill-emacs)
(recursive-edit)
