;; https://gist.github.com/kosh04/568800/raw/d3f0313cf0b80e87c7ed44d74bcc11f66427c054/%5Bemacs%2Cxyzzy%5Dunicode-%7B%2Cun%7Descape-region
;; #+:Emacs
(defun unicode-unescape-region (start end)
  "指定した範囲のUnicodeエスケープ文字(\\uXXXX)をデコードする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\u\\([[:xdigit:]]\\{4\\}\\)" nil t)
      (replace-match (string (unicode-char
                              (string-to-number (match-string 1) 16)))
                     nil t))))

(defun unicode-escape-region (&optional start end)
  "指定した範囲の文字をUnicodeエスケープする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "." nil t)
      (replace-match (format "\\u%04x"
                             (char-unicode
                              (char (match-string 0) 0)))
                     nil t))))

;; #+:Emacs
;; こちらも参照→ http://github.com/kosh04/emacs-lisp > xyzzy.el
(defun char-unicode (char) (encode-char char 'ucs))
(defun unicode-char (code) (decode-char 'ucs code))

(defun pick-normalize-path (path)
  (replace-regexp-in-string (format "^%s" (getenv "HOME")) "~" path))

(defun pick-current-directory-name () (interactive)
  (let1 source (pick-normalize-path default-directory)
    (kill-new source)
    (message "copy: %s" source)))

(defun pick-current-word () (interactive)
  (let1 source (current-word)
    (kill-new source)
    (message "copy: %s" source)))

(defun pick-current-file-name () (interactive)
  (let1 source (pick-normalize-path buffer-file-name)
    (kill-new source)
    (message "copy: %s" source)))

(defun browse-current-file () (interactive)
  (let1 source buffer-file-name
    (browse-url source)))

(defun open-shell-with-pwd () (interactive)
  (let1 dir (current-directory)
    (shell)
    (comint-simple-send (get-buffer-process dir)
                        (concat "cd " dir))
    (goto-char (point-max))))

(defun find-decreased-file () (interactive)
  (let ((fname buffer-file-name)
        (num-rx "[0-9]+"))
    (when (string-match num-rx fname)
      (let* ((num (- (string-to-number (match-string 0 fname)) 1))
             (decreased-filename 
              (replace-regexp-in-string num-rx (number-to-string num) fname)))
        (find-file decreased-filename)))))

(defun find-increased-file () (interactive)
  (let ((fname buffer-file-name)
        (num-rx "[0-9]+"))
    (when (string-match num-rx fname)
      (let* ((num (+ 1 (string-to-number (match-string 0 fname))))
             (increased-filename 
              (replace-regexp-in-string num-rx (number-to-string num) fname)))
        (find-file increased-filename)))))


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

(defun font-at-cp () (interactive)
  (print (font-at (point))))
