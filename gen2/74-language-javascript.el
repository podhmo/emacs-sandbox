;; (require-and-fetch-if-not 'flymake-jshint)
;; (require-and-fetch-if-not 'flymake-easy)
;; (require-and-fetch-if-not 'flymake-eslint :url "https://raw.githubusercontent.com/tjefferson08/flymake-eslint/master/flymake-eslint.el")

(defun my:js-insert-comma () (interactive)
  (insert ",")
  (unless (looking-at-p "$")
    (insert " ")))

(setq my:js-key-pair
      '(("(" . ")")
        ("\"" . "\"")
        ("`" . "`")
        ("'" . "'")
        ("{"  "}" "{")
        ("[" "]" "["))
      )

(setq my:js-key-map
      `(
        ("," . my:python-insert-comma)
        ))

(defun my:js-setup ()
  (setq indent-tabs-mode nil
        js-indent-offset 2
        tab-width 2)
  (setq js-indent-level 2)
  ;; (flymake-jshint-load) ;hmm
  ;; (flymake-eslint-load) ;hmm
  ;; (flymake-mode 1)
  (define-many-keys js-mode-map my:js-key-map)
  (define-insert-pair-binding js-mode-map my:js-key-pair))

(add-hook 'js-mode-hook 'my:js-setup)

;; typescript
(autoload 'typescript-mode "typescript" nil t)
(with-eval-after-load 'typescript
  ;; (require-and-fetch-if-not 'typescript)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))

  (defun my:typescript-setup ()
    (flycheck-mode t)
    (setq typescript-indent-level 2)
    (define-many-keys typescript-mode-map my:js-key-map)
    (define-insert-pair-binding typescript-mode-map my:js-key-pair))

  (add-hook 'typescript-mode-hook 'my:typescript-setup)
  )

;; es6

(define-derived-mode es6-mode
  javascript-mode "es6"
  "Major mode for javascript(es6).
\\{es6-mode-map}"
  (setq case-fold-search nil))

(add-to-list 'auto-mode-alist '("\\.es6$" . es6-mode))
(add-to-list 'quickrun-file-alist '("\\.es6" . "javascript/es6"))
(add-to-list 'quickrun--language-alist
             '("javascript/es6"
               (:command . (lambda () (pickup-file "node_modules/.bin/babel-node")))
               (:compile-only . (lambda () (format "%s %%s" (pickup-file "node_modules/.bin/babel"))))
               (:description . "Run es6 file")))

;; check
;; (quickrun--decide-file-type "hello.es6")
;; (quickrun--command-key "hello.es6")
;; (pop quickrun--language-alist)

