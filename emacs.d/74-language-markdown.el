(use-package markdown-mode
  :ensure t
  :defer
  :commands (markdown-mode)
  :mode (("\\.md" . markdown-mode))
  :init
  (defun my:markdown-setup ()
    (define-key markdown-mode-map (kbd "C-c C-c C-c") 'toggle-file-mode))
  :config
  (add-hook 'markdown-mode-hook 'my:markdown-setup))




