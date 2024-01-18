;;(require 'elm-mode) ;;autoload

(defun my-elm-setup ()
  (define-insert-pair-binding elm-mode-map my-elm-key-pair)
  )

(setq my-elm-key-pair
      '(("(" . ")")
        ("\"" . "\"")
        ("'" . "'")
        ("{"  "}" "{")
        ("[" "]" "[")))


(eval-after-load 'elm-mode
  (lambda ()
    (add-hook 'elm-mode-hook 'my-elm-setup)
    )
  )
