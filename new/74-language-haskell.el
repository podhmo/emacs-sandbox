;; this is haskell settings
;; sudo port install ghc hs-cabal
;; cabal-0.14.0 update
;; cabal-0.14.0 install cabal-install
;; cabal-0.14.0 install ghc-mod hlint

(require-and-fetch-if-not 'haskell-mode)
(require-and-fetch-if-not 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) 

(defun my:haskell-cabal-home ()
  (concat (getenv "HOME") "/.cabal"))

(add-to-list 'exec-path (concat (my:haskell-cabal-home) "/bin"))



(defun my:haskell-setup ()
  (ghc-init)
  (flymake-mode)
  )
(add-hook 'haskell-mode-hook 'my:haskell-setup)

(add-to-list 'load-path (concat (current-directory) "ghc-mod"))
(autoload 'ghc-init "ghc" nil t)
