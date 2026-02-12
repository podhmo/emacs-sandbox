;;; -*- coding: utf-8; lexical-binding: t -*-

;;----------------------------------------
;; text
;;----------------------------------------


(defun my:delete-something () (interactive)
       (cl-dolist (thing '(symbol word whitespace))
	     (when-let ((found (thing-at-point thing)))
	       (cl-return (delete-region (beginning-of-thing thing)  (end-of-thing thing ))))))

(defun my:enclose-quote (beg end)
  "foo -> \"foo\""
  (interactive
   (list
    (if (use-region-p) (region-beginning) (beginning-of-thing 'word))
    (if (use-region-p) (region-end) (end-of-thing 'word))))
  (save-restriction
    (let ((text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (prin1-to-string text))))  ;; prin1-to-string ha tenuki
  )

(defun my:ansi-color-strip ()
  "for github action's log .etc"
  (interactive)
  (save-excursion
    (while (re-search-forward "\\\\[[0-9]*m" nil t 1)
      (replace-match ""))))


;;----------------------------------------
;; shell
;;----------------------------------------

(defun my:shell-command-on-region-with-kill-new (start end command &optional output-buffer replace error-buffer display-error-buffer region-noncontiguous-p)
  "`shell-command-on-region'のkill-ringに追加する版"
  ;; ここはshell-command-on-regionのinteractiveのコードそのまま
  (interactive (let (string)
                 (unless (mark)
		           (user-error "The mark is not set now, so there is no region"))
		         (setq string (read-shell-command "Shell command on region: "))
		         (list (region-beginning) (region-end) string current-prefix-arg current-prefix-arg shell-command-default-error-buffer t (region-noncontiguous-p))))

  (let ((out-buffer-name shell-command-buffer-name))
    (shell-command-on-region start end command output-buffer replace out-buffer-name out-buffer-name region-noncontiguous-p)
    (unless replace
      (let ((output-string
             (with-current-buffer shell-command-buffer-name
               (buffer-substring (point-min) (point-max))
               )))
        (message output-string)
        (kill-new output-string)))))

(provide 'utils)
