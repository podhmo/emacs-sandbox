(require-and-fetch-if-not 'paredit)
(define-key paredit-mode-map (kbd "C-j") ctl-j-map)

(defun my:emacs-lisp-setup ()
  (define-many-keys emacs-lisp-mode-map
    '(("C-c C-j" . lisp-complete-symbol)
      ("C-c M-r" . paredit-forward-slurp-sexp)
      ("C-c M-R" . paredit-forward-barf-sexp)
      ("C-c M-l" . paredit-backward-slurp-sexp)
      ("C-c M-L" . paredit-backward-barf-sexp)
      ))
  (turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (paredit-mode +1)
  )    
(add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-setup))
