(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
      default-directory))

(add-to-list 'load-path (current-directory))
(add-to-list 'load-path (concat (current-directory) "/3rdparty"))

;; marmalade
(require 'package)
(setq package-user-dir (concat (current-directory) "/3rdparty"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;; (package-list-packages)