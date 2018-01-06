(defun my:jsonfmt (beg end)
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min))
    (setq end (point-max))
    )
  (my:execute-formatter-command "jq" "jq . -S -e" beg end))

(require 'derived)
(define-derived-mode my:json-mode javascript-mode "json mode")

(defun my:json-mode-setup ()
  (define-key my:json-mode-map (kbd "C-x C-s") 'my:jsonfmt)
  )

(add-to-list 'auto-mode-alist '("\\.json$" . my:json-mode))
(add-hook 'my:json-mode-hook 'my:json-mode-setup)
