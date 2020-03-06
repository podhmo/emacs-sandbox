;; org-mode
(require 'org-install)
;; (require 'org-remember)
(add-to-list 'auto-mode-alist '("\\.notes$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (add-to-list 'auto-mode-alist '("memo[0-9]+\\.txt" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(defun my:org-setup ()
  (define-key org-mode-map (kbd "C-c C-c") 'toggle-file)
  (define-key org-mode-map (kbd "C-j") nil)
)
(add-hook 'org-mode-hook 'my:org-setup)

(defun remember-insert-top-of-the-file ()
  (let ((text (buffer-string))
        (desc (remember-buffer-desc)))
    (with-temp-buffer
      (insert remember-leader-text (current-time-string)
              " (" desc ")\n\n" text)
      (let ((remember-text (buffer-string)))
        (with-current-buffer (find-file-noselect remember-data-file)
          (save-excursion
            (goto-char (point-min))
            (insert remember-text "\n")
            (goto-char (point-min))
            (when remember-save-after-remembering (save-buffer)))))
      )))
(setq remember-handler-functions '(remember-insert-top-of-the-file))

;; org-agenda
(setq calendar-holidays nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
(setq org-log-done 'time)

(setq org-agenda-custom-commands
      '(("f" occur-tree "FIXME")))

