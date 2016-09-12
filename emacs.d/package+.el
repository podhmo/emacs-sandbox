(require 'cl)
(defmacro* require-and-fetch-if-not (package &key (filename nil) (noerror t) (installed-package nil) (url nil))
  (let ((pname (gensym)))
    `(or (require ,package ,filename t)
         (let ((,pname (or ,installed-package ,package))
               (my:package-install-url ,url))
           (package-install ,pname)
           (require ,package ,filename t)))))

(require 'package)

;; obsolete
(defun package--dir (name &optional version)
  (let ((version (or version "0.0")))
    (format "%s/%s-%s/" package-user-dir name version)))


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

(unless (fboundp 'package-handle-response)
  (defun package-handle-response ()
    "Handle the response from a `url-retrieve-synchronously' call.
Parse the HTTP response and throw if an error occurred.
The url package seems to require extra processing for this.
This should be called in a `save-excursion', in the download buffer.
It will move point to somewhere in the headers."
    ;; We assume HTTP here.
    (require 'url-http)
    (let ((response (url-http-parse-response)))
      (when (or (< response 200) (>= response 300))
        (error "Error during download request:%s"
               (buffer-substring-no-properties (point) (progn
                                                         (end-of-line)
                                                         (point)))))))
  )

(unless (fboundp 'package-unpack-single)
  (defun selfish:version-to-list (version)
    (condition-case e
        (version-to-list version)
      (error (version-to-list "0.0.0")))) ;;

  (defun package-unpack-single (file-name version desc requires)
    "Install the contents of the current buffer as a package."
    ;; Special case "package".
    (if (string= file-name "package")
        (package--write-file-no-coding
         (expand-file-name (concat file-name ".el") package-user-dir))
      (let* ((pkg-dir  (expand-file-name (concat file-name "-"
                                                 (package-version-join
                                                  (selfish:version-to-list version)))
                                         package-user-dir))
             (el-file  (expand-file-name (concat file-name ".el") pkg-dir))
             (pkg-file (expand-file-name (concat file-name "-pkg.el") pkg-dir)))
        (make-directory pkg-dir t)
        (package--write-file-no-coding el-file)
        (let ((print-level nil)
              (print-length nil))
          (write-region
           (concat
            (prin1-to-string
             (list 'define-package
                   file-name
                   version
                   desc
                   (list 'quote
                         ;; Turn version lists into string form.
                         (mapcar
                          (lambda (elt)
                            (list (car elt)
                                  (package-version-join (cadr elt))))
                          requires))))
            "\n")
           nil
           pkg-file
           nil nil nil 'excl))
        (package-generate-autoloads file-name pkg-dir)
        (let ((load-path (cons pkg-dir load-path)))
          (byte-recompile-directory pkg-dir 0 t)))))
)

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
    (let ((version* (package-version-join (selfish:version-to-list version))))
      ;; Try to activate it.
      (add-to-list 'load-path (package--dir name version))
      (when my:local-package-sync-p
        (add-to-list 'my:local-package-list (list name version))
        (my:local-package-store-save)))))

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

(unless (fboundp 'package-name-or-desc)
  (defun package-name-or-desc (&rest args))
  )
(unless (boundp 'package-name-or-desc)
  (setq package-name-or-desc nil)
  )
(provide 'package+)
