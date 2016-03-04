(require 'markdown-mode)

(defun my:markdown-setup ()
  (define-key markdown-mode-map (kbd "C-c C-c C-c") 'toggle-file-mode))

(eval-after-load 'markdown-mode
  (lambda ()
    (add-hook 'markdown-mode-hook 'my:markdown-setup))
  )

