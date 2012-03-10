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

(defun define-many-keys (key-map key-table)
  (loop for (key . cmd) in key-table
        do (define-key key-map (read-kbd-macro key) cmd)))

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
              (load "human-interfaces")
              (keybord-settings-setup)
              ))
  (load "auto-save")
  (add-hook 'after-init-hook
            (lambda () 
              (auto-save-buffers-start 0.5))))

(named-progn keyboad-settings
  (defvar on-after-keybord-setup (list))
  (defvar on-before-keybord-setup (list))
  (defmacro with-before-keybord-setup (&rest body)
    `(add-hook 
      'on-before-keybord-setup
      (lambda ()
        ,@body)))
  (defmacro with-after-keybord-setup (&rest body)
    `(add-hook 
     'on-after-keybord-setup
     (lambda ()
       ,@body)))

  (defvar ctl-j-map (make-keymap))
  (global-set-key (kbd "C-j") ctl-j-map)
  
  (defvar keybord-settings:curdir (current-directory))
  (defun keybord-settings-setup ()
    ;; occur before settings hook
    (run-hook-with-args-until-failure
     'on-before-keybord-setup)

    (named-progn global-key-settings ;; or human-interfaces.el
      (setq global-individual-key-mapping
            '(("C-c C-l" . eval-buffer)
              ("M-r" . replace-string)
              ("M-R" . replace-regexp)
              ("C-c C-f" . ffap)
              ("C-c C-e" . eval-defun) ;;
              ("C-c j" . dabbrev-expand)
              ("C-c C-j" . dabbrev-expand) ;;
              ("C-c q" . comment-region)
              ("C-c Q" . uncomment-region)
              
              ("C-c x" . (lambda () (interactive) (find-file (concat keybord-settings:curdir "/init.el"))))
              ("C-j S" . elscreen-shell/next-screen)
              
              ("C-;" . elscreen-previous)
              ("C-:" . elscreen-next)))
      (define-many-keys (current-global-map) global-individual-key-mapping))

    (ffap-bindings) ;; url also enable when typed C-x C-f

    ;; occur after settings hook
    (run-hook-with-args-until-failure
     'on-after-keybord-setup)
    ))

(named-progn emacsclient
  ;; emacsclient サーバを起動
  (condition-case err
      (progn
        (autoload 'server-running-p "server") 
        (unless (server-running-p)  (server-start)))
    (error (message "emacsclient load fail"))))

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

(named-progn treat-dumped-junks
  (defvar junks-directory-path "~/junks")
  (defvar junks-directory-force-create-p t)

  (defun junks-create-directory-if-force (force-p)
    (unless (and forcep
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

(named-progn programming-languages
  (named-progn emacs-lisp
    (defun my:emacs-lisp-setup ()
      (define-many-keys emacs-lisp-mode-map
        '(("C-c C-j" . lisp-complete-symbol)
          ))
      (turn-on-eldoc-mode)
      (eldoc-add-command
       'paredit-backward-delete
       'paredit-close-round)
      ;; (paredit-mode +1)
      )    
    (add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-setup))
  ;;(require 'paredit)

  (named-progn python
    (defun my:python-setup ()
      (autopair-on))
    
    (require-and-fetch-if-not 'python-mode)
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    (add-hook 'python-mode-hook 'my:python-setup)
    )
  )

(run-hook-with-args 'after-init-hook)
