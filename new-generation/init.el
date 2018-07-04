;; package management
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(setq my:current-dir (current-directory))
(setq package-user-dir (format "%s%s" (current-directory) "elpa"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; use package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  )
