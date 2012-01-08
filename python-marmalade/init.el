(require 'cl)
;; utility
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
      default-directory))

(defmacro comment (&rest body)
  nil)

(defmacro* require-if-not-fetch (package &key (filename nil) (noerror t) (installed-package nil))
  (let ((pname (gensym)))
    `(unless (require ,package ,filename t)
       (let ((,pname (or ,installed-package ,package)))
	 (package-install ,pname)))))
;;

(add-to-list 'load-path (current-directory))
(add-to-list 'load-path (concat (current-directory) "/3rdparty"))

;; marmalade
(require 'package)
(setq package-user-dir (concat (current-directory) "/3rdparty"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; get available packages
(unless package-archive-contents
  (package-refresh-contents))
(package-initialize)

;; (package-list-packages)
(require-if-not-fetch 'python-mode)
(find-file (concat (current-directory) "init.el"))