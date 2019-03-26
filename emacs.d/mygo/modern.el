;; with-eglot and ivy
(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)

  (defun my:ivy-mode-setup ()
    (setq completing-read-function 'completing-read-default)
    )
  (add-hook 'ivy-mode-hook 'my:ivy-mode-setup)
  )

(use-package counsel
  :ensure t
  :bind (;("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable))
  :config
  ;; (setq ffap-file-finder 'counsel-find-file)
)

(use-package go-mode
  :defer t
  :ensure t
  :commands (go-mode)
  :mode (("\\.go" . go-mode))
  :init

  (with-eval-after-load 'eglot
    (unless (getenv "GOPATH")
      (setenv "GOPATH" (my:go-path)))
    (add-to-list 'exec-path (format "%s/bin" (or (getenv "GOPATH") "~/go")))
    ;; go get -v -u github.com/saibing/bingo
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
    (my:eglot-ensure)
    (ivy-mode 1)
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
