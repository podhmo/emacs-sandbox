(require 'cl)
;; utility
(setq debug-on-error nil)

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(defmacro comment (&rest body)
  nil)

(defmacro named-progn (name &rest body)
  (declare (indent 1))
  `(progn ,@body))

(defmacro* require-and-fetch-if-not (package &key (filename nil) (noerror t) (installed-package nil) (url nil))
  (let ((pname (gensym)))
    `(or (require ,package ,filename t)
         (let ((,pname (or ,installed-package ,package))
               (my:package-install-url ,url))
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
              (named-progn experimental
                (load "change-mode-hook"))
              ))
  (load "auto-save")
  (add-hook 'after-init-hook
            (lambda () 
              (auto-save-buffers-start 0.5))))


(named-progn package-management
  (require 'package)
  (setq package-user-dir (concat (current-directory) "3rdparty"))
  (load "package+")
  
  (named-progn marmalade
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    ;; get available packages
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    ;; (package-list-packages)
    )) 


(named-progn keyboad-settings
  (named-progn key-chord
    (require-and-fetch-if-not 'key-chord :url "http://www.emacswiki.org/emacs/download/key-chord.el")
    (setq key-chord-two-keys-delay 0.04)
    (key-chord-mode 1)
    )

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
              ("C-c e" . enclose-element-interactive)
              ("C-c d" . delete-syntax-forward*)
              ("C-j S" . elscreen-shell/next-screen)
              
              ("C-;" . elscreen-previous)
              ("C-:" . elscreen-next)
              ("<f5>" . revert-buffer))
              )
      (define-many-keys (current-global-map) global-individual-key-mapping))

    (ffap-bindings) ;; url also enable when typed C-x C-f

    (named-progn key-chord
      (key-chord-define-global "jk" 'view-mode)
      (key-chord-define-global "po" 'org-remember))

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

(named-progn programming-languages

  (named-progn quick-run
    (require-and-fetch-if-not 'quickrun :url "https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el")
    )

  (named-progn font-lock-language-plugin
    ;; (defmacro language:define-plugin (name args &rest body)
    ;;   (declare (indent 2))
    ;;   `(defun* ,name ,args 
    ;;      (add-to-list 'language:activated-plugins ',name)
    ;;      ,@body))
    (font-lock-add-keywords
     'emacs-lisp-mode 
     '(("(\\([^\t ]*?:define-plugin\\) " (1 font-lock-keyword-face) 
        ("[^\t ]+?" nil nil (0 font-lock-function-name-face))))))
  
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

  (named-progn yasnipet ;;move-it
    (require-and-fetch-if-not 'yasnippet)
    (yas/load-directory (concat (current-directory) "3rdparty/yasnippet-20120320/snippets")))
  
  (named-progn python 
    (load "language.python")

    (named-progn activate-plugin
      (python:auto-mode-alist-plugin)
      (python:flymake-plugin)
      (python:autopair-plugin)
      (python:strict-indent-plugin)
      (python:flymake-eldoc/current-position-plugin)
      (python:run-program-plugin-simple)
      (python:yasnippet-plugin)
      )

    (defun my:python-setup ()
      (run-hooks 'python:plugin-mode-hook)

      (when (python:plugin-activate-p 'python:run-program-plugin-simple)
        (define-many-keys python-mode-map
          '(("C-c @" . python:run-program-current-buffer)))))

    (add-hook 'python-mode-hook 'my:python-setup))

  (named-progn ruby
    (require-and-fetch-if-not 'flymake-ruby)
    (require 'ruby-mode nil t)
    (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  )
)

(run-hook-with-args 'after-init-hook)
