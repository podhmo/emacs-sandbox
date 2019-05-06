;; with-eglot and ivy

(use-package go-mode
  :defer t
  :ensure t
  :commands (go-mode)
  :mode (("\\.go" . go-mode))
  :init

  (with-eval-after-load 'eglot
    (unless (getenv "GOPATH")
      (setenv "GOPATH" (replace-regexp-in-string "~" (getenv "HOME") (my:go-path))))
    (add-to-list 'exec-path (format "%s/bin" (or (getenv "GOPATH") "~/go")))
    ;; go get -v -u github.com/saibing/bingo
    ;; (add-to-list 'eglot-server-programs '(go-mode "bingo" "-format-style" "goimports"))
    ;; go get -u -v golang.org/x/tools/cmd/gopls
    (add-to-list 'eglot-server-programs '(go-mode "gopls"))
    )

  (defvar my:golang-key-pair
    '(("(" . ")")
      ("\"" . "\"")
      ("'" . "'")
      ("{"  "}" "{")
      ("[" "]" "["))
    )

  (defun my:go-mode-setup ()
    (my:eglot-ensure)
    (define-insert-pair-binding go-mode-map my:golang-key-pair)

    ;; TODO: eldoc more fancy UI?
    ;; flymake TODO: too heavy?
    (bind-keys :map go-mode-map
               ("C-x C-s" . gofmt) ;; TODO: remove (tentative)
               ;; ("C-x C-s" . eglot-format-buffer)
               ("M-." . xref-find-definitions)
               ("M-," . pop-tag-mark)
               ("C-c C-j" . xref-find-definitions)
               ("C-M-i" . completion-at-point) ;; TODO: more fancy UI?
               )
    (my:go-setup-format-buffer)
    )
  (add-hook 'go-mode-hook 'my:go-mode-setup)
  :config
  (my:go-setup-gofmt-command) ;; TODO: remove (tentative)
  )
