;;; Begin 0
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;;; Begin 0.5 this is temporary
(add-to-list 'load-path (concat (current-directory) "3rdparty"))

(load (concat (current-directory) "package+"))

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


;;; Begin 1
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load (current-directory))


;;; Begin 2

(put 'narrow-to-region 'disabled nil)
(setq debug-on-error nil)

(defun current-memo ()
  (let* ((memos (directory-files2 "~/vboxshare/memo" t))
         (max-n-files-pair
          (reduce (lambda (r x)
                    (cond ((string-match "\\([0-9]+\\)\\.txt$" x)
                           (let1 n (string-to-int (match-string-no-properties 1 x))
                             (if (> n (car r)) (cons n x) r)))
                          (t r)))
                  memos
                  :initial-value (cons 0 "memo0.txt"))))
    (cdr max-n-files-pair)))

;;; do-execute
  (progn
    (auto-save-buffers-start 0.5)

    (ffap-bindings) ;; url also enable when typed C-x C-f

    ;; occur after settings hook
    (keyboard-settings-setup)
    (run-hook-with-args-until-failure 'on-after-keyboard-setup)
    (find-file (current-memo))
    (toggle-file-mode)
    )

(run-hook-with-args 'after-init-hook)

;;; Begin 3 individual ad-hoc setting for-mac
(require 'cl)
(define-key global-map "¥" (lambda (&optional n) (interactive "p") (dotimes (i (or n 1))  (insert "\\"))))
(setq-default tab-stop-list (loop for i from 4 to 120 by 4
                                  collect i))

(setq-default ring-bell-function
              (lambda () (message "ding")))

;; temporary
;; (defun setup-sp-toggle-on-org ()
;;   (define-key org-mode-map "\C-c\C-c" 'toggle-file)
;;   )
;; (add-hook 'org-mode-hook 'setup-sp-toggle-on-org)

(let ((pair (rassoc 'image-file-handler file-name-handler-alist)))
  (and pair
       (setcar pair "\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|tiff?\\|x\\(?:[bp]m\\)\\)\\'"))
  )

;; gauche
(add-to-list 'Info-default-directory-list "/opt/local/share/info")
;(setenv "INFOPATH" "/opt/local/share/info/:$INFOPATH")
(put 'upcase-region 'disabled nil)
