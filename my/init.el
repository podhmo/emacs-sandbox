(require 'cl)
;; utility
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
      default-directory))

(defmacro comment (&rest body)
  nil)

(defmacro* require-and-fetch-if-not (package &key (filename nil) (noerror t) (installed-package nil))
  (let ((pname (gensym)))
    `(unless (require ,package ,filename t)
       (let ((,pname (or ,installed-package ,package)))
	 (package-install ,pname)))))
;;

(add-to-list 'load-path (current-directory))
(add-to-list 'load-path (concat (current-directory) "/3rdparty"))

;; my-base setting
(load "util")
(load "base-settings")
(load "daily-commands")
(load "layout")
(load "human-interfaces")


;; marmalade
(require 'package)
(setq package-user-dir (concat (current-directory) "/3rdparty"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; get available packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;;;;;;;;; ;;
;;; emacsclient サーバを起動
;; (when (and (require 'server nil t) (not (server-running-p)))
;;   (server-start))
(autoload 'server-running-p "server") 
(unless (server-running-p)  (server-start))

;; recentf
(setq recentf-max-saved-items 500)
(recentf-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)




;; (package-list-packages)
(require-and-fetch-if-not 'python-mode)
(find-file (concat (current-directory) "init.el"))