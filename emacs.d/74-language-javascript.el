(require-and-fetch-if-not 'flymake-jshint)

(setq my:js-key-pair
      '(("(" . ")")
        ("\"" . "\"")
        ("'" . "'")
        ("{"  "}" "{")
        ("[" "]" "["))
      )

(defun my:js-setup ()
  (setq indent-tabs-mode nil
        js-indent-offset 2
        tab-width 2)
  (setq js-indent-level 2)
  (flymake-jshint-load) ;hmm
  (flymake-mode 1)
  (define-insert-pair-binding js-mode-map my:js-key-pair))

(add-hook 'js-mode-hook 'my:js-setup)

