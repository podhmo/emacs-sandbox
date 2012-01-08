(defun current-hook-change-to-empty () (interactive)
  (let ((hook (symbol-at-point)))
    (cond ((boundp hook)
           (unwind-protect
               (progn
                 (describe-variable hook)
                 (when (yes-or-no-p (format "%s -> nil? :" hook))
                   (eval `(setq ,hook nil))
                   (message "%s -> nil!" hook)))
             (dolist (w (get-buffer-window-list (help-buffer)))
               (delete-window  w))))
          (t (message "no bound found -%s-" hook)))))
