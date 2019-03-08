(cond
 ((and (require 'eglot nil t) nil)
  (load (format "%smygo/modern.el" (current-directory))))
 (t
  (load (format "%smygo/legacy.el" (current-directory)))
  ))
