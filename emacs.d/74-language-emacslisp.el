(require 'paredit)
(define-key paredit-mode-map (kbd "C-j") ctl-j-map)


(defun* elisp-output-with-comment (&key (place-holder "; => "))
  (interactive)
  (let ((output (call-interactively 'eval-last-sexp)))
    (let ((output-found
           (save-excursion
             (goto-char (point-at-eol))
             (search-backward place-holder (point-at-bol) t 1))))
      (when output-found
        (delete-region output-found (point-at-eol)))
      (save-excursion
        (goto-char (point-at-eol))
        (insert place-holder (prin1-to-string output))))))

(defun my:emacs-lisp-setup ()
  (define-many-keys emacs-lisp-mode-map
    '(("C-c C-j" . lisp-complete-symbol)
      ("C-c M-r" . paredit-forward-slurp-sexp)
      ("C-c M-R" . paredit-forward-barf-sexp)
      ("C-c M-l" . paredit-backward-slurp-sexp)
      ("C-c M-L" . paredit-backward-barf-sexp)
      
      ("C-c C-i" . elisp-output-with-comment)
      ))
  (turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (paredit-mode +1)
  )    
(add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-setup))
