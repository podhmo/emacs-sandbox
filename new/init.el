;;; Begin 0
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(add-to-list 'load-path (current-directory))


;;; begin 1
(require 'cl)
(defmacro* require-and-fetch-if-not (package &key (filename nil) (noerror t) (installed-package nil) (url nil))
  (let ((pname (gensym)))
    `(or (require ,package ,filename t)
         (let ((,pname (or ,installed-package ,package))
               (my:package-install-url ,url))
           (package-install ,pname)
           (require ,package ,filename t)))))

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


;;; begin 2
(defvar ctl-j-map (make-keymap))
(defun define-many-keys (key-map key-table)
  (loop for (key . cmd) in key-table
        do (define-key key-map (read-kbd-macro key) cmd)))

;;; begin 3

(add-to-list 'load-path (current-directory))
(add-to-list 'load-path (concat (current-directory) "/3rdparty"))

(progn ;; package-management
  (require 'package)
  (setq package-user-dir (concat (current-directory) "3rdparty"))

  (progn ;; marmalade
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    ;; get available packages
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    ;; (package-list-packages)
    )) 

;; init-loader
(require-and-fetch-if-not 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load (current-directory))

;; code-block
(require-and-fetch-if-not 'config-block :url "https://gist.github.com/podhmo/3917706/raw/3bd74df6fbc69995be57f4635d4b14f2afeedaa4/config-block.el")

;; this is ad-hoc settings for mac
(define-key global-map "¥" (lambda (&optional n) (interactive "p") (dotimes (i (or n 1))  (insert "\\"))))
(setq-default tab-stop-list (loop for i from 4 to 120 by 4
                                  collect i))

(setq-default ring-bell-function
              (lambda () (message "ding")))

(put 'narrow-to-region 'disabled nil)
(setq debug-on-error nil)

;; (require-and-fetch-if-not 'elpy)
;; (elpy-enable)

;;; initialize
;;; do-execute
  (progn
    (auto-save-buffers-start 0.5)

    (ffap-bindings) ;; url also enable when typed C-x C-f

    ;; occur after settings hook
    (keyboard-settings-setup)
    (run-hook-with-args-until-failure 'on-after-keyboard-setup)

    (let* ((memos (directory-files2 "~/vboxshare/memo" t))
           (max-n-files-pair
            (reduce (lambda (r x)
                      (cond ((string-match "\\([0-9]+\\)\\.txt$" x)
                             (let1 n (string-to-int (match-string-no-properties 1 x))
                               (if (> n (car r)) (cons n x) r)))
                                             (t r)))
                    memos
                    :initial-value (cons 0 "memo0.txt"))))
      (find-file (cdr  max-n-files-pair)))
    )

(run-hook-with-args 'after-init-hook)
