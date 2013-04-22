;;; from url
;; (let ((my:package-install-url "https://....<package-name>.el"))
;;   (package-install '<package-name>))
;;; from package list
;; (package-install '<package-name>

(require 'package)

;; dont-use, directly
(defvar my:package-install-url nil)
(defvar my:local-package-list nil)

;; custom variable 
(defvar my:local-package-sync-p t)

;; utility
(defmacro* my:package--with-work-buffer (url &rest body)
  "inspired by `package--with-work-buffer'"
  (declare (indent 1))
  (let ((buf (gensym)))
    `(let ((,buf  (url-retrieve-synchronously ,url)))
       (with-current-buffer ,buf
         (progn 
           (package-handle-response)
           (goto-char (point-min))
           (re-search-forward "^$" nil t)
           (delete-region (point-min) (+ 1 (point))))
         ,@body)
       (kill-buffer ,buf))))

(defun my:package--find-version ()
  (save-excursion
    (progn (goto-char (point-min))
           (re-search-forward "Version: *" nil t 1)
           (or (thing-at-point 'symbol) "0.0"))))

;; functions
(defun my:package-install-from-url (url name &optional version description requires)
  (let ((version version))
    (my:package--with-work-buffer url
      (unless version
        (setq version (my:package--find-version)))
      (package-unpack-single name version (or description "no description")
                             requires))
    ;; Try to activate it.
    (add-to-list 'load-path (package--dir name version))
    (when my:local-package-sync-p
      (add-to-list 'my:local-package-list (list name version))
      (my:local-package-store-save))))

;; local package
(defun my:local-package-store--create (fname &optional forcep)
  (when (or (not (file-exists-p fname)) forcep)
    (with-current-buffer (find-file-noselect fname)
      (insert "nil")
      (save-buffer))))
  
(defun my:local-package-store-fname ()
  (concat package-user-dir "/.local-package.list"))

(defun my:local-package-store-load ()
  (let ((fname (my:local-package-store-fname)))
    ;; if not found. create store file
    (my:local-package-store--create fname)
    (let ((buf (find-file-noselect fname)))
      (prog1 (read (with-current-buffer buf (buffer-string)))
        (kill-buffer buf)))))

(defun my:local-package-store-save ()
  (let ((fname (my:local-package-store-fname)))
    (with-current-buffer (find-file-noselect fname)
      (erase-buffer)
      (insert (prin1-to-string my:local-package-list))
      (save-buffer)
      (kill-buffer))))

(defun my:local-package-initialize ()
  (loop for (name version) in (my:local-package-store-load)
        do (add-to-list 'load-path (package--dir name version))))

;; advices
(defadvice package-install (around from-url-dispatch last (name) activate)
  (cond (my:package-install-url
         (let ((name (if (symbolp name) (symbol-name name) name)))
           (my:package-install-from-url my:package-install-url name)))
        (t ad-do-it)))

(defadvice package-initialize (after local-package-initialize activate)
  (my:local-package-initialize))
