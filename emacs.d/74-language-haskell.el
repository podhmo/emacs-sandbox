;; this is haskell settings
;; sudo port install ghc hs-cabal
;; cabal-0.14.0 update
;; cabal-0.14.0 install cabal-install
;; cabal-0.14.0 install ghc-mod hlint

(require 'haskell-mode)
(require 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) 

(defun my:haskell-cabal-home ()
  (concat (getenv "HOME") "/.cabal"))

(add-to-list 'exec-path (concat (my:haskell-cabal-home) "/bin"))

(when (require 'insert-pair-element nil t)  
  (setq haskell-selfish:key-pair
        '(("(" . ")")
          ("\"" . "\"")
          ("'" . "'")
          ("{"  "}" "{")
          ("[" "]" "["))))

(defun my:haskell-setup ()
  (flymake-mode)
  (ghc-init)
  (turn-on-haskell-doc-mode)
  ;; (turn-on-haskell-indentation)
  (turn-on-haskell-simple-indent)
  (define-insert-pair-binding haskell-mode-map haskell-selfish:key-pair)
  (define-key haskell-mode-map (kbd "\C-c \C-@") 'haskell-hoogle)
  )
(add-hook 'haskell-mode-hook 'my:haskell-setup)
(require 'ghc)

