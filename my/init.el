(require 'cl)
;; utility
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(defmacro comment (&rest body)
  nil)

(defmacro named-progn (name &rest body)
  (declare (indent 1))
  `(progn ,@body))

(defmacro* require-and-fetch-if-not (package &key (filename nil) (noerror t) (installed-package nil))
  (let ((pname (gensym)))
    `(or (require ,package ,filename t)
         (let ((,pname (or ,installed-package ,package)))
           (package-install ,pname)
           (require ,package ,filename t)))))
;;
(add-hook 'after-init-hook
          (lambda ()
            (find-file (concat (current-directory) "init.el"))))

(add-to-list 'load-path (current-directory))
(add-to-list 'load-path (concat (current-directory) "/3rdparty"))


(named-progn my-base-settings
  (load "util")
  (load "base-settings")
  (load "daily-commands")
  (load "layout")
  (add-hook 'after-init-hook
            (lambda ()
              (load "human-interfaces")))
  (load "auto-save")
  (add-hook 'after-init-hook
            (lambda () 
              (auto-save-buffers-start 0.5))))

(named-progn emacsclient
  ;; emacsclient サーバを起動
  ;; (when (and (require 'server nil t) (not (server-running-p)))
  ;;   (server-start))
  (autoload 'server-running-p "server") 
  (unless (server-running-p)  (server-start)))

(named-progn recentf
  (setq recentf-max-saved-items 500)
  (recentf-mode 1))

(named-progn uniquify
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))


(named-progn marmalade
  (require 'package)
  (setq package-user-dir (concat (current-directory) "/3rdparty"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  ;; get available packages
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  ;; (package-list-packages)
  )

(named-progn elscreen
  (require-and-fetch-if-not 'elscreen)

  (defun global-j-define-key (&optional kmap)
    (and-let* ((kmap (or kmap (current-local-map))))
      (define-key kmap "\C-j" nil))) ;; experimental
  
  (defadvice elscreen-goto (after kill-Cj  activate)
    (global-j-define-key))
  (defadvice switch-to-buffer (after kill-Cj  activate)
    (global-j-define-key))

  (defvar ctl-j-map (make-keymap))
  (global-set-key (kbd "C-j") ctl-j-map)
  (setq elscreen-prefix-key (kbd "C-j"))

  (elscreen-start)
  )

(require-and-fetch-if-not 'autopair)
;; (named-progn anything
;;   (require-and-fetch-if-not 

(named-progn python
  (defun my:python-setup ()
    (autopair-on))

  (require-and-fetch-if-not 'python-mode)
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  (add-hook 'python-mode-hook 'my:python-setup)
  )

(run-hook-with-args 'after-init-hook)