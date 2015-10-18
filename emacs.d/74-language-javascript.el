;; (require-and-fetch-if-not 'flymake-jshint)
(require-and-fetch-if-not 'flymake-easy)
(require-and-fetch-if-not 'flymake-eslint :url "https://raw.githubusercontent.com/tjefferson08/flymake-eslint/master/flymake-eslint.el")

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
  ;;(flymake-jshint-load) ;hmm
  (flymake-eslint-load) ;hmm
  (flymake-mode 1)
  (define-insert-pair-binding js-mode-map my:js-key-pair))

(add-hook 'js-mode-hook 'my:js-setup)

;; typescript
(require-and-fetch-if-not 'typescript)
(defun my:typescript-setup ()
  (setq indent-tabs-mode nil
        js-indent-offset 2
        tab-width 2)
  (setq js-indent-level 2)
  (define-insert-pair-binding typescript-mode-map my:js-key-pair))

(add-hook 'typescript-mode-hook 'my:typescript-setup)

