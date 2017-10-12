(defun my:get-fresh-buffer-create (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setf (buffer-string) ""))
    buf
    ))

(defun my:execute-formatter-command (cmd-name cmd beg end)
  (cond ((executable-find cmd-name)
         (save-excursion
           (save-restriction
             (narrow-to-region beg end)
             (let ((buf (my:get-fresh-buffer-create (format "*%s*" cmd-name)))
                   (err-buf (my:get-fresh-buffer-create (format "*%s error*" cmd-name))))
               (let ((status
                      ;; xxx
                      (flet ((display-message-or-buffer (&rest args) nil))
                        (shell-command-on-region (point-min) (point-max) cmd buf nil err-buf)
                        )))
                 (cond ((= 0 status)
                        (let ((replaced (with-current-buffer buf (buffer-string))))
                          (cond ((string= replaced "")
                                 (message "succeeded with no output"))
                                (t
                                 (delete-region (point-min) (point-max))
                                 (insert replaced)))))
                       (t (message (with-current-buffer err-buf (buffer-string))))))))))
        (t (message (format "%s is not found" cmd-name)))))

(defun my:jsonfmt (beg end)
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min))
    (setq end (point-max))
    )
  (my:execute-formatter-command "jq" "jq . -S -e" beg end))

(require 'derived)
(define-derived-mode my:json-mode javascript-mode "json mode")

(defun my:json-mode-setup ()
  (define-key my:json-mode-map (kbd "C-x C-s") 'my:jsonfmt)
  )

(add-to-list 'auto-mode-alist '("\\.json$" . my:json-mode))
(add-hook 'my:json-mode-hook 'my:json-mode-setup)
