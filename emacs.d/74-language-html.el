(setq my:html-key-pair
      '(("(" . ")")
        ("\"" . "\"")
        ("'" . "'")
        ("<"  ">" "<")
        ("{"  "}" "{")
        ("[" "]" "["))
      )

(defun my:html-setup ()  
  (define-key html-mode-map [?¥] [?\\])
  (define-insert-pair-binding html-mode-map my:html-key-pair))

(add-hook 'html-mode-hook 'my:html-setup)
; (add-to-list 'auto-mode-alist '("\\.phtml$" . html-mode))
