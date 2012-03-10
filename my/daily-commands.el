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

(defun elscreen-shell/next-screen () (interactive)
  "create shell buffer with current directory as pwd"
  (let1 dir (current-directory)
    (elscreen-create)
    (shell)
    (comint-simple-send (get-buffer-process dir)
                        (concat "cd " dir))
    (goto-char (point-max))))

(named-progn for-follow-mode
  (defvar split-window-function
    'split-window-horizontally)
  
  (defun follow-with-n-window (n) (interactive "p")
    "start follow-mode with n-window. n is prefix argument.
so. c-u 3 follow-with-n-window, then a frame splitted 3window
"
    (let ((wins (count-windows))
          (cur-buf (current-buffer))
          (n (if (= n 1) 2 n)))
      (when (> n wins)
        (dotimes (i (- n wins))
          (funcall split-window-function)))
      (save-window-excursion
        (dolist (w (window-list))
          (select-window w)
          (switch-to-buffer cur-buf)))
      (turn-on-follow-mode))))
