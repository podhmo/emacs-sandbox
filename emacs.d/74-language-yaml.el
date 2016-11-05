(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(with-eval-after-load 'yaml-mode
  (require 'flymake-yaml)
  (add-hook 'yaml-mode-hook 'flymake-mode)
  )
