;;(setq merlin-command (executable-find "ocamlmerlin")) ; needed only if ocamlmerlin not already in your PATH
(require-and-fetch-if-not 'tuareg)
;;(require-and-fetch-if-not 'auto-complete)
(autoload 'merlin-mode "merlin" "Merlin mode" t)

(when (executable-find "ocaml-toplevel")
  (setq tuareg-interactive-program "ocaml-toplevel"))

(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)
