(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(setq my:csharp-key-pair
      '(("(" . ")")
        ("\"" . "\"")
        ("'" . "'")
        ("[" "]" "["))
      )

(defun my:csharp-setup ()
  ;; indentation
  (define-insert-pair-binding csharp-mode-map my:csharp-key-pair)
  )

(add-hook 'csharp-mode-hook 'my:csharp-setup)
