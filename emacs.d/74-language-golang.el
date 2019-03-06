(cond
 ((require 'eglot nil t)
  (load (format "%smygo/modern.el" (current-directory))))
 (t
  (load (format "%smygo/legacy.el" (current-directory)))
  ))
