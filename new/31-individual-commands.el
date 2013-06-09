(defun open-shell-with-pwd () (interactive)
  (let1 dir (current-directory)
    (shell)
    (comint-simple-send (get-buffer-process dir)
                        (concat "cd " dir))
    (goto-char (point-max))))

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

(progn ;; for-follow-mode
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


(progn ;; treat-dumped-junks
  (defvar junks-directory-path "~/junks")
  (defvar junks-directory-force-create-p t)

  (defun junks-create-directory-if-force (force-p)
    (unless (and force-p
                 (file-exists-p junks-directory-path) 
                 (file-directory-p junks-directory-path))
      (make-directory junks-directory-path)))

  (defun junks-insert-content (strings)
    (save-excursion
      (goto-char (point-max))
      (insert "\n\n")
      (dolist (s strings)
        (insert s "\n"))))

  (defun move-junks (&rest contents) 
    (let* ((timestamp (format-time-string "%Y-%m-%d" (current-time)))
           (fname (format "%s/junks.%s" junks-directory-path timestamp)))
      (junks-create-directory-if-force  junks-directory-force-create-p)
      (with-current-buffer (find-file-noselect fname)
        (junks-insert-content contents))))

  (defun move-junks-region (beg end comment) (interactive "r\nscomment:")
    (move-junks
     comment
     (delete-and-extract-region beg end)))
  )


(defun simple-timer (n d &optional color) (interactive "nwait
ndelay")
  (run-with-timer 
   n nil 
   (lexical-let ((d d) (color (or color "#8d8d8d")))
     (lambda (&rest args)
       (lexical-let ((original-color (background-color-at-point)))
         (set-background-color color)
         (run-with-timer d nil (lambda (&rest args) (set-background-color original-color)))
         )))))
