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

  (defvar ctl-j-map (make-keymap))
  (global-set-key (kbd "C-j") ctl-j-map)
  
  (defvar keybord-settings:curdir (current-directory))
  (defun keybord-settings-setup ()
    ;; occur before settings hook
    (run-hook-with-args-until-failure
     'on-before-keybord-setup)

    (named-progn global-key-settings
      (setq global-individual-key-mapping
            '(("C-c C-l" . eval-buffer)
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

  (add-hook 'on-before-keybord-setup
            (lambda ()
              (defadvice elscreen-goto (after kill-Cj  activate)
                (global-j-define-key))
              (defadvice switch-to-buffer (after kill-Cj  activate)
                (global-j-define-key))
              (setq elscreen-prefix-key (kbd "C-j"))
              (elscreen-start)))
  )

(named-progn editing
  (require-and-fetch-if-not 'autopair)
  (require-and-fetch-if-not 'paredit)
  )

(named-progn eye-candy
  (require 'eldoc)
  (setq eldoc-argument-case 'downcase)

  (defadvice  eldoc-get-fnsym-args-string
    (around eldoc-named-progn-display-section (sym &optional index) activate)
    (cond ((eq sym 'named-progn)
           (let ((section-name
                  (save-excursion
                    (eldoc-beginning-of-sexp)
                    (goto-char (scan-sexps (point) 1))
                    (skip-syntax-forward "^w_")
                    (thing-at-point 'symbol))))
             (message "named-progn -- %s --" section-name)))
          (t ad-do-it)))
  )

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