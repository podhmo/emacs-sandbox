(defun char-unicode (char) (encode-char char 'ucs))
(defun unicode-char (code) (decode-char 'ucs code))

(defun pick-normalize-path (path)
  (replace-regexp-in-string (format "^%s" (getenv "HOME")) "~" path))

(defun pick-current-directory-name ()
  (interactive)
  (let1 source (pick-normalize-path default-directory)
    (kill-new source)
    (message "copy: %s" source)))

(defun pick-current-word ()
  (interactive)
  (let1 source (current-word)
    (kill-new source)
    (message "copy: %s" source)))

(defun pick-current-file-name ()
  (interactive)
  (let1 source (pick-normalize-path buffer-file-name)
    (kill-new source)
    (message "copy: %s" source)))

(defun kill-no-exists-file-buffers ()
  (interactive)
  (loop for b in (buffer-list)
        unless (or (string-match-p "^ *\\*" (buffer-name b))
                   (and (buffer-file-name b) (file-exists-p (buffer-file-name b))))
        do (kill-buffer b)))

(defun browse-current-file ()
  (interactive)
  (let1 source buffer-file-name
    (browse-url source)))

(defun open-shell-with-pwd ()
  (interactive)
  (let1 dir (current-directory)
    (shell)
    (comint-simple-send (get-buffer-process dir)
                        (concat "cd " dir))
    (goto-char (point-max))))

(cl-defun find-increased-file (&key (n 1) (default 1))
  (interactive)
  (let* ((fname (file-name-nondirectory buffer-file-name))
         (num-rx "[0-9]+")
         (new-name fname))
    (cond ((string-match num-rx fname)
           (let* ((s (match-string 0 fname))
                  (num (+ (string-to-number s) n))
                  (new-s (format (format "%%0%dd" (length s)) num)))
             (setq new-name
                   (replace-regexp-in-string num-rx new-s fname))))
          (t
           (setq new-name
                 (replace-regexp-in-string
                  (regexp-quote (file-name-sans-extension fname))
                  (format "%s%d" (file-name-sans-extension fname) default)
                   fname))))
    (find-file (concat (file-name-directory buffer-file-name) new-name))))

(defun find-decreased-file ()
  (interactive)
  (find-increased-file :n -1 :default 1))


(defun current-hook-change-to-empty ()
  (interactive)
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

  (defun follow-with-n-window (n)
    (interactive "p")
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


(defun simple-timer (n d &optional color)
  (interactive "nwait\nndelay")
  (run-with-timer
   n nil
   (lexical-let ((d d) (color (or color "#8d8d8d")))
     (lambda (&rest args)
       (lexical-let ((original-color (background-color-at-point)))
         (set-background-color color)
         (run-with-timer d nil (lambda (&rest args) (set-background-color original-color)))
         )))))

(defun font-at-current-point ()
  (interactive)
  (print (font-at (point))))

;;
(defun my:refresh-process-forcely ()
  (interactive)
  (dolist (p (process-list)) (delete-process p))
  (server-start))

;; TODO: package
(require 'subr-x)
(defvar my:masking-buffer-candidates-alist
  `(
    (,(lambda ()
        (when-let* ((it (pickup:pickup-file "/bin/pip")))
          (replace-regexp-in-string "/bin/pip$" "" it)))
     . "VENV")
    (,(getenv "GOPATH") . "$GOPATH")
    (,(getenv "HOME") . "$HOME")
    ))

(defun my:masking-buffer (beg end)
  "あんまり見せたくないpathなどをmaskする"
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min))
    (setq end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (cl-loop
       for (pattern . replacement) in my:masking-buffer-candidates-alist
       do
       (progn
         (when-let* ((it (if (functionp pattern) (funcall pattern) pattern)))
           (message "MASKING-APPLY %s %s" it replacement)
           (goto-char (point-min))
           (while (search-forward it nil t 1)
             (replace-match replacement nil t))))))))

(defun my:strip-ansi-color-on-region (beg end)
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min))
    (setq end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\[[0-9]+;?[0-9]*m" nil t 1)
        (replace-match "")
        ))))

(defun my:shell-command-on-region-and-insert (start end command)
  (interactive
   (let (string)
     (unless (mark)
	   (user-error "The mark is not set now, so there is no region"))
	 (setq string (read-shell-command "Shell command on region(and insert): "))
	 (list (region-beginning) (region-end) string))
   )
  (let ((output-buffer t)
        (replace t))
    (shell-command-on-region start end command output-buffer replace)))

(defun my:resolve-github-url (&optional path)
  (when-let* ((git-config-file (pickup:pickup-file ".git/config" :path path)))
    (cl-block b
      (let ((buf (find-file-noselect git-config-file)))
        (with-current-buffer buf
          (goto-char (point-min))
          (when (re-search-forward "url = git@github.com:\\(.+\\)\\(?:\\.git\\)?" nil t)
            (let ((user-repository-name (string-trim-right (match-string-no-properties 1) "\\.git$")))
              (cl-return-from b (format "https://github.com/%s" user-repository-name))))
          (goto-char (point-min))
          (when (re-search-forward "url = https://github.com/\\(.+\\)\\(?:\\.git\\)?" nil t)
            (let ((user-repository-name (string-trim-right (match-string-no-properties 1) "\\.git$")))
              (cl-return-from b (format "https://github.com/%s" user-repository-name))))
          )))))

(cl-defun my:browse-github (&key (branch nil) (rel-path nil) (line-no nil))
  (interactive)
  (destructuring-bind (_ . path) (project-current)
    (when-let* ((url (my:resolve-github-url path)))
      (let* ((branch (or branch (car (vc-git-branches))))
             (rel-path (or rel-path (replace-regexp-in-string (expand-file-name path) "" (buffer-file-name))))
             (line-no (or line-no (line-number-at-pos)))
             (full-url (format "%s/blob/%s/%s#L%d" url branch rel-path line-no)))
        (browse-url full-url))))
  )
(defalias 'browse-github 'my:browse-github)

(defun browse-github-master ()
  (interactive)
  (browse-github :branch "master"))

(defun my:ipsum ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
  )
