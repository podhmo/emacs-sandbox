;; with-eglot
(use-package go-mode
  :defer t
  :commands (go-mode)
  :mode (("\\.go" . go-mode))
  :init

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(go-mode "bingo" "-format-style" "goimports"))
    )

  (defvar my:golang-key-pair
    '(("(" . ")")
      ("\"" . "\"")
      ("'" . "'")
      ("{"  "}" "{")
      ("[" "]" "["))
    )

  (defun my:go-mode-setup ()
    (eglot-ensure)
    (define-insert-pair-binding go-mode-map my:golang-key-pair)

    ;; TODO: eldoc more fancy UI?
    ;; flymake TODO: too heavy?
    (bind-keys :map go-mode-map
               ("C-x C-s" . eglot-format-buffer)
               ("M-." . xref-find-definitions)
               ("M-," . pop-tag-mark)
               ("C-c C-j" . xref-find-definitions)
               ("C-M-i" . completion-at-point) ;; TODO: more fancy UI?
               )
    )
  (add-hook 'go-mode-hook 'my:go-mode-setup)
  :config
  )
