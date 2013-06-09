(require 'cl)
;; utility
(setq debug-on-error t)

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))
(defmacro comment (&rest body)
  nil)

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
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (find-file (concat (current-directory) "init.el"))))

(add-to-list 'load-path (current-directory))
(add-to-list 'load-path (concat (current-directory) "/3rdparty"))


(progn ;; my-base-settings
  (load "util")
  (load "base-settings")
  (load "daily-commands")
  (load "layout")
  (add-hook 'after-init-hook
            (lambda ()
              (load "human-interfaces")
              (keybord-settings-setup)
              (progn ;; experimental
                (load "change-mode-hook"))
              ))
  (load "auto-save")
  (add-hook 'after-init-hook
            (lambda () 
              (auto-save-buffers-start 0.5))))


(progn ;; package-management
  (require 'package)
  (setq package-user-dir (concat (current-directory) "3rdparty"))
  (load "package+")
  
  (progn ;; marmalade
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    ;; get available packages
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    ;; (package-list-packages)
    )) 

(require-and-fetch-if-not 'anything)

(progn ;; keyboad-settings
  (progn ;; key-chord
    (require-and-fetch-if-not 'key-chord :url "http://www.emacswiki.org/emacs/download/key-chord.el")
    (setq key-chord-two-keys-delay 0.01)
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

    (progn ;; global-key-settings ;; or human-interfaces.el
      (setq global-individual-key-mapping
            '(("C-c C-l" . eval-buffer)
              ("M-r" . replace-string)
              ("M-R" . replace-regexp)

              ("C-x C-l" . goto-line)

              ("C-c C-c" . toggle-file)
              ("C-c C-f" . ffap)
              ("C-c C-e" . eval-defun) ;;
              ("C-c j" . dabbrev-expand)
              ("C-c C-j" . dabbrev-expand) ;;
              ("C-c q" . comment-region)
              ("C-c Q" . uncomment-region)
              
              ("C-c x" . (lambda () (interactive) (find-file (concat keybord-settings:curdir "/init.el"))))
              ("C-c e" . enclose-element-interactive)
              ("C-c d" . delete-syntax-forward*)
              ("M-r" . replace-string)
              ("M-R" . replace-regexp)

              ;; quick-run
              ("C-c @" . quickrun-compile-only)
              ("C-c C-@" . quickrun)

              ;elscreen
              ("C-;" . elscreen-previous)
              ("C-:" . elscreen-next)
              ("C-j S" . elscreen-shell/next-screen)
              ("C-j C-f" . elscreen-find-file)              

              ("C-." . redo)
              ("C-/" . undo)
              ("C-j S" . open-shell-with-pwd)
              ("<f5>" . revert-buffer)
              ("<f12>" . (lambda () (interactive)
                           (message "reflesh")
                           (setq extended-command-history nil)))
              )
              )
      (define-many-keys (current-global-map) global-individual-key-mapping))

    (ffap-bindings) ;; url also enable when typed C-x C-f

    (progn ;; key-chord
      (key-chord-define-global "jk" 'view-mode)
      (key-chord-define-global "po" 'org-remember))

    ;; occur after settings hook
    (run-hook-with-args-until-failure
     'on-after-keybord-setup)
    ))

(progn ;; anything-git
  (require-and-fetch-if-not 'with-prefix :url "https://raw.github.com/podhmo/anything-vcs-project.el/master/with-prefix.el")
  (require-and-fetch-if-not 'anything-vcs-project :url "https://raw.github.com/podhmo/anything-vcs-project.el/master/anything-vcs-project.el")
  (setq anything-vcs-project:cache-enable-p t)
  (setq anything-vcs-project:cache.project-list-path "~/.emacs.d/.project.list")
  (setq anything-vcs-project-git:exclude-args "--exclude .hg --exclude '*.pyc'")
  (setq anything-vcs-project-hg:exclude-args "--exclude .git --exclude '*.pyc'")
  (global-set-key (kbd "C-c C-:") 'anything-vcs-project)
)

(progn ;; emacsclient
  ;; emacsclient サーバを起動
  (condition-case err
      (progn
        (autoload 'server-running-p "server") 
        (unless (server-running-p)  (server-start)))
    (error (message "emacsclient load fail"))))

(progn ;; recentf
  (setq recentf-max-saved-items 500)
  (recentf-mode 1))

(progn ;; uniquify
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(progn ;; toggle-file-mode
  (require-and-fetch-if-not 'toggle-file-mode :url "https://raw.github.com/gist/2600353/9a5d6965bc075c2c967b8fbb832cc4800abc14dc/toggle-file-mode.el"))
  
;; (progn ;; speedbar
;;   (require-and-fetch-if-not 'sr-speedbar)
;;   )

;; code-block
(require-and-fetch-if-not 'config-block :url "https://gist.github.com/podhmo/3917706/raw/3bd74df6fbc69995be57f4635d4b14f2afeedaa4/config-block.el")

;; python
;; (load "language.python-new")
;; (config-block-setup! 
;;  '(
;;    addon:python.core
;;    addon:python.strict.indent
;;    addon:python.find-python
;;    addon:python.import.ffap
;;    addon:python.quickrun
;;    addon:python.hook.initialize
;;    ))

(progn ;; programming-languages
  (progn ;; quick-run
    (require-and-fetch-if-not 'quickrun :url "https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el")
    (defadvice quickrun (around help-mode-after-quickrun activate)
      (lexical-let ((before-buf (current-buffer)))
        ad-do-it
        (when (and (not (equal before-buf (current-buffer)))
                   (eq major-mode 'fundamental-mode))
          (help-mode))))
    )

  (progn ;; font-lock-language-plugin
    ;; (defmacro language:define-plugin (name args &rest body)
    ;;   (declare (indent 2))
    ;;   `(defun* ,name ,args 
    ;;      (add-to-list 'language:activated-plugins ',name)
    ;;      ,@body))
    (font-lock-add-keywords
     'emacs-lisp-mode 
     '(("(\\([^\t ]*?:define-plugin\\) " (1 font-lock-keyword-face) 
        ("[^\t ]+?" nil nil (0 font-lock-function-name-face))))))
  
  (progn ;; emacs-lisp
    (require-and-fetch-if-not 'paredit)
    (define-key paredit-mode-map (kbd "C-j") ctl-j-map)

    (defun my:emacs-lisp-setup ()
      (define-many-keys emacs-lisp-mode-map
        '(("C-c C-j" . lisp-complete-symbol)
          ("C-c M-r" . paredit-forward-slurp-sexp)
          ("C-c M-R" . paredit-forward-barf-sexp)
          ("C-c M-l" . paredit-backward-slurp-sexp)
          ("C-c M-L" . paredit-backward-barf-sexp)
          ))
      (turn-on-eldoc-mode)
      (eldoc-add-command
       'paredit-backward-delete
       'paredit-close-round)
      (paredit-mode +1)
      )    
    (add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-setup))

  ;; (progn ;; yasnipet ;;move-it
  ;;   (require-and-fetch-if-not 'yasnippet)
  ;;   (yas/load-directory (concat (current-directory) "3rdparty/yasnippet-20120320/snippets")))
  
  (progn ;; auto-complete ;;move-it
    (require-and-fetch-if-not 'auto-complete)
    (setq ac-use-menu-map t)
    (define-key ac-menu-map "\C-n" 'ac-next)
    (define-key ac-menu-map "\C-p" 'ac-previous)
    )

  (progn ;; html
    (add-hook 'html-mode-hook ;; move-it
              (lambda ()
                (autopair-on)
                (modify-syntax-entry ?% "w_"))))
  (progn ;; scheme
    (load "language.scheme"))

  (progn ;; python 
    (load "language.python")

    (progn ;; activate-plugin
      (python:auto-mode-alist-plugin)
      (python:flymake-plugin)
      (python:autopair-plugin)
      (python:strict-indent-plugin)
      (python:flymake-eldoc/current-position-plugin)
      (python:anything-with-modules-plugin)
      ;; (python:yasnippet-plugin)
      ;; (python:run-program-plugin-simple)
      (python:quickrun-with-virtualenv-plugin)
      (python:auto-complete-plugin)
      (python:swap-backquote-and-underscore-plugin)
      )

    (defun my:python-setup ()
      (run-hooks 'python:plugin-mode-hook)

      (let1 keymaps
          (loop for (plugin . key-maps) in
                '((python:run-program-plugin-simple
                   . (("C-c @" . python:run-program-current-buffer)))
                  (python:anything-with-modules-plugin
                   . (("C-c C-f" . python:anything-with-modules)
                      ("C-c f" . python-describe-symbol))))
                when (python:plugin-activate-p plugin)
                append key-maps)
        (define-many-keys python-mode-map keymaps)
        (define-key python-mode-map "\C-c\C-c" 'toggle-file)
        ))

    (add-hook 'python-mode-hook 'my:python-setup))

  (progn ;; ruby
    (require-and-fetch-if-not 'flymake-ruby)
    (require 'ruby-mode nil t)
    (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  )
)

(run-hook-with-args 'after-init-hook)

(defadvice anything-M-x (before keep-history-size-more-than-one activate)
    (while (null (cdr extended-command-history))
      (push "*dummy*" extended-command-history)))

;; this is ad-hoc settings for mac
(define-key global-map "¥" (lambda (&optional n) (interactive "p") (dotimes (i (or n 1))  (insert "\\"))))
(setq-default tab-stop-list (loop for i from 4 to 120 by 4
                                  collect i))

(setq-default ring-bell-function
              (lambda () (message "ding")))

;; this is haskell settings
;; sudo port install ghc hs-cabal
;; cabal-0.14.0 update
;; cabal-0.14.0 install cabal-install
;; cabal-0.14.0 install ghc-mod hlint
(require-and-fetch-if-not 'haskell-mode)
(require-and-fetch-if-not 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) 

(defun my:haskell-cabal-home ()
  (concat (getenv "HOME") "/.cabal"))

(add-to-list 'exec-path (concat (my:haskell-cabal-home) "/bin"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(org-agenda-files (quote ("~/org/sample.org")))
 '(remember-handler-functions (quote (remember-insert-top-of-the-file))))

(defun my:haskell-setup ()
  (ghc-init)
  (flymake-mode)
  )
(add-hook 'haskell-mode-hook 'my:haskell-setup)

(add-to-list 'load-path (concat (current-directory) "ghc-mod"))
(autoload 'ghc-init "ghc" nil t)

(put 'narrow-to-region 'disabled nil)
(setq debug-on-error nil)

(defun simple-timer (n d &optional color) (interactive "nwait
ndelay")
  (run-with-timer 
   n nil 
   (lexical-let ((d d) (color (or color "#8d8d8d")))
     (lambda (&rest args)
       (lexical-let ((original-color (background-color-at-point)))
         (set-background-color color)
         (run-with-timer d nil (lambda (&rest args) (set-background-color original-color)))
         )))))

;; org-mode
(require 'org-install)
(require 'org-remember)
(add-to-list 'auto-mode-alist '("\\.notes$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("memo[0-9]+\\.txt" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun remember-insert-top-of-the-file ()
  (let ((text (buffer-string))
        (desc (remember-buffer-desc)))
    (with-temp-buffer
      (insert remember-leader-text (current-time-string)
              " (" desc ")\n\n" text)
      (let ((remember-text (buffer-string)))
        (with-current-buffer (find-file-noselect remember-data-file)
          (save-excursion
            (goto-char (point-min))
            (insert remember-text "\n")
            (goto-char (point-min))
            (when remember-save-after-remembering (save-buffer)))))
      )))
(setq remember-handler-functions '(remember-insert-top-of-the-file))

;; org-agenda
(setq calendar-holidays nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
(setq org-log-done 'time)

(setq org-agenda-custom-commands
      '(("f" occur-tree "FIXME")))

