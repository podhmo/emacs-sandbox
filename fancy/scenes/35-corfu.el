;;; 35-corfu -- in-buffer completion popup with kind-icon
(let ((b (get-buffer-create "*corfu-demo*")))
  (switch-to-buffer b)
  (delete-other-windows)
  (emacs-lisp-mode)
  (insert "(mapc")
  ;; corfu exhibits from post-command-hook just like vertico
  (completion-at-point)
  (run-hooks 'post-command-hook))
(run-at-time
 0.8 nil
 (lambda ()
   (fancy:shoot (getenv "FANCY_OUT") 'window)
   (kill-emacs)))
(run-at-time 20 nil #'kill-emacs)
(recursive-edit)
