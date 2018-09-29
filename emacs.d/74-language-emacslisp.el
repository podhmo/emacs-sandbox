(require 'paredit)
(define-key paredit-mode-map (kbd "C-j") ctl-j-map)


(defun* my:elisp-output-with-comment (&key (place-holder ";; => "))
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

(defun my:elisp-format (start end arg)
  (interactive "r\nP")
  (unless (region-active-p)
    (save-excursion
      (end-of-defun)
      (setq end (point))
      (beginning-of-defun)
      (setq start (point))
      )
    )

  (save-excursion
    (save-restriction
      (condition-case err
          (progn
            (narrow-to-region start end)
            (goto-char (point-min))
            (indent-region (point-min) (point-max) arg)

            ;; trim white space of eol
            (goto-char (point-min))
            (while (re-search-forward "[ 	]+$" nil t 1)
              (replace-match "")
              )
            )
        (error (widen) (message err))
        )
      ))
  )

(defun my:emacs-lisp-setup ()
  (define-many-keys emacs-lisp-mode-map
    '(("C-c C-j" . lisp-complete-symbol)
      ("C-c M-r" . paredit-forward-slurp-sexp)
      ("C-c M-R" . paredit-forward-barf-sexp)
      ("C-c M-l" . paredit-backward-slurp-sexp)
      ("C-c M-L" . paredit-backward-barf-sexp)
      ("C-c C-i" . my:elisp-output-with-comment)
      ("C-x C-s" . my:elisp-format)
      ))

  (when (require 'company nil t)
    (company-mode)
    (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)
    )
  (turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (paredit-mode +1)
  )
(add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-setup)
