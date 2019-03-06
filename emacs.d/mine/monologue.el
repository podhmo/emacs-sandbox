;;; monologue.el --- tiny monologue. -*- lexical-binding: t; -*-
(require 'time-stamp)

(defvar monologue:current-buffer-name-function 'current-memo)
(defvar monologue:monologue-partation-string
  "-monologue----------------------------------------\n")

(defun monologue:get-current-buffer ()
  (let ((bufname (funcall monologue:current-buffer-name-function)))
    (find-file-noselect bufname)))

(defun monologue:start-point ()
  (goto-char (point-max))
  (let ((paration-string monologue:monologue-partation-string))
    (cond ((re-search-backward paration-string nil t 1) (forward-line 1))
          (t (progn
               (goto-char (point-max))
               (insert paration-string))))))


(defun monologue:header-default ()
  (format "%s [%s]\n" 
          (time-stamp-string "%Y/%:m/%:d %:H:%:M")
          (or (buffer-file-name) (buffer-name)))
  )

(defvar monologue:header-function 'monologue:header-default)
(defun monologue:header ()
  (funcall monologue:header-function))

(defun monologue (message) (interactive "smessage: ")
       (let ((buf (monologue:get-current-buffer))
             (header (monologue:header)))
         (with-current-buffer buf
           (save-excursion
             (monologue:start-point)
             (insert header)
             (insert message)
             (insert "\n")
             (unless (string-match "\n$" message)
               (insert "\n"))))))
(provide 'monologue)
;;; monologue.el ends here
