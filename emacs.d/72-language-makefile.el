(with-eval-after-load 'make-mode
  (defun my:makefile-disable-warn-suspicious-lines ()
    (remove-hook 'write-file-functions 'makefile-warn-suspicious-lines t))
  (add-hook 'makefile-mode-hook 'my:makefile-disable-warn-suspicious-lines)
  )
