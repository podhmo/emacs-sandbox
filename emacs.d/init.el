(require 'cl)
;;; Begin 0

(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ))

;;; Begin 0.25 custom environment setting (loading git-ignored file if existed)
(progn
  (defgroup my nil
    "My custom settings"
    :prefix "my:")

  (defcustom my:keyboard-layout 'ja
    "My keyboard layout setting. This can be `en' or `ja'."
    :type '(choice (const en) (const ja))
    :group 'my
    )

  (let ((target-file (concat (current-directory) "00custom-environment.el")))
    (when (file-exists-p target-file)
      (message "%s is existedd, loading" target-file)
      (load-file target-file))
    )
)


;;; Begin 0.5 this is temporary
(add-to-list 'load-path (concat (current-directory) "3rdparty"))
(add-to-list 'load-path (concat (current-directory) "mine"))

(load (concat (current-directory) "package+"))

(progn ;; package-management
  (require 'package)
  (setq package-user-dir (concat (current-directory) "3rdparty"))

  (progn
    ;; get available packages
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    ;; (package-list-packages)
    )

  (setq use-package-compute-statistics t)
  (require-and-fetch-if-not 'use-package)
  )


;(load (concat (current-directory) "init-visualize")) ;; hmm
;(initchart-visualize-init-sequence "/tmp/b.svg")

;;; Begin 1
(use-package init-loader
  :ensure t
  :config
  (setq init-loader-show-log-after-init nil)

  ;; suppress Warinig
  (setq *curdir* (current-directory))
  (defadvice init-loader-load (after suppress-warning activate)
    (setq load-path
          (remove-if (lambda (x) (string-equal x *curdir*)) load-path))
    )

  (init-loader-load (current-directory))
)



;;; Begin 2
(put 'narrow-to-region 'disabled nil)
(setq debug-on-error nil)

(unless (fboundp 'string-to-int)
  (defalias 'string-to-int 'string-to-number))

(defun current-memo ()
  (let* ((memos (directory-files2 "~/vboxshare/memo" t))
         (max-n-files-pair
          (reduce (lambda (r x)
                    (cond ((string-match "\\([0-9]+\\)\\.txt$" x)
                           (let1 n (string-to-int (match-string-no-properties 1 x))
                             (if (> n (car r)) (cons n x) r)))
                          (t r)))
                  memos
                  :initial-value (cons -1 "memo0.txt"))))
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

(let ((pair (rassoc 'image-file-handler file-name-handler-alist)))
  (and pair
       (setcar pair "\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|tiff?\\|x\\(?:[bp]m\\)\\)\\'"))
  )

(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-yank-pop-separator "
----------------------------------------
")
 '(custom-safe-themes
   '("81c3de64d684e23455236abde277cda4b66509ef2c28f66e059aa925b8b12534" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default))
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-command-wrapper-function #'my:flycheck-wrapper-function--use-fullpath-command)
 '(flycheck-executable-find #'my:flycheck-executable-find)
 '(flymake-no-changes-timeout nil)
 '(flymake-start-on-flymake-mode t)
 '(flymake-start-on-newline nil)
 '(flymake-start-on-save-buffer t)
 '(help-at-pt-display-when-idle '(flymake-overlay) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(highlight-indent-guides-auto-enabled t)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive nil)
 '(package-selected-packages
   '(typescript-mode solarized-theme which-key-posframe ivy-postframe python-environment dracula-theme counsel anything-match-plugin anything-config anything zlc go-eldoc elscreen bm eglot jsonrpc json-rpc dash-functional lsp-mode shackle racer company-racer company-jedi epc scala-mode disable-mouse flycheck-rust rust-mode fcitx "flymake-yaml" flymake-yaml toggle-file-mode pickup initchart flymake-jshint flymake-eslint ffap-python company-go anything-vcs-project yaml-mode with-editor which-key volatile-highlights use-package undo-tree transient terraform-mode quickrun posframe pkg-info paredit markdown-mode magit key-chord ivy-rich ivy-posframe init-loader hcl-mode git-commit flycheck epl dash bind-key async))
 '(python-environment-virtualenv (list "python" "-m" "venv" "--system-site-packages"))
 '(safe-local-variable-values
   '((fci-rule-column . 140)
     (c-comment-only-line-offset 0 . 0)
     (eval progn
           (defun my/point-in-defun-declaration-p nil
             (let
                 ((bod
                   (save-excursion
                     (c-beginning-of-defun)
                     (point))))
               (<= bod
                   (point)
                   (save-excursion
                     (goto-char bod)
                     (re-search-forward "{")
                     (point)))))
           (defun my/is-string-concatenation-p nil "Returns true if the previous line is a string concatenation"
                  (save-excursion
                    (let
                        ((start
                          (point)))
                      (forward-line -1)
                      (if
                          (re-search-forward " \\+$" start t)
                          t nil))))
           (defun my/inside-java-lambda-p nil "Returns true if point is the first statement inside of a lambda"
                  (save-excursion
                    (c-beginning-of-statement-1)
                    (let
                        ((start
                          (point)))
                      (forward-line -1)
                      (if
                          (search-forward " -> {" start t)
                          t nil))))
           (defun my/trailing-paren-p nil "Returns true if point is a training paren and semicolon"
                  (save-excursion
                    (end-of-line)
                    (let
                        ((endpoint
                          (point)))
                      (beginning-of-line)
                      (if
                          (re-search-forward "[ ]*);$" endpoint t)
                          t nil))))
           (defun my/prev-line-call-with-no-args-p nil "Return true if the previous line is a function call with no arguments"
                  (save-excursion
                    (let
                        ((start
                          (point)))
                      (forward-line -1)
                      (if
                          (re-search-forward ".($" start t)
                          t nil))))
           (defun my/arglist-cont-nonempty-indentation
               (arg)
             (if
                 (my/inside-java-lambda-p)
                 '+
               (if
                   (my/is-string-concatenation-p)
                   16
                 (unless
                     (my/point-in-defun-declaration-p)
                   '++))))
           (defun my/statement-block-intro
               (arg)
             (if
                 (and
                  (c-at-statement-start-p)
                  (my/inside-java-lambda-p))
                 0 '+))
           (defun my/block-close
               (arg)
             (if
                 (my/inside-java-lambda-p)
                 '- 0))
           (defun my/arglist-close
               (arg)
             (if
                 (my/trailing-paren-p)
                 0 '--))
           (defun my/arglist-intro
               (arg)
             (if
                 (my/prev-line-call-with-no-args-p)
                 '++ 0))
           (c-set-offset 'inline-open 0)
           (c-set-offset 'topmost-intro-cont '+)
           (c-set-offset 'statement-block-intro 'my/statement-block-intro)
           (c-set-offset 'block-close 'my/block-close)
           (c-set-offset 'knr-argdecl-intro '+)
           (c-set-offset 'substatement-open '+)
           (c-set-offset 'substatement-label '+)
           (c-set-offset 'case-label '+)
           (c-set-offset 'label '+)
           (c-set-offset 'statement-case-open '+)
           (c-set-offset 'statement-cont '++)
           (c-set-offset 'arglist-intro 'my/arglist-intro)
           (c-set-offset 'arglist-cont-nonempty
                         '(my/arglist-cont-nonempty-indentation c-lineup-arglist))
           (c-set-offset 'arglist-close 'my/arglist-close)
           (c-set-offset 'inexpr-class 0)
           (c-set-offset 'access-label 0)
           (c-set-offset 'inher-intro '++)
           (c-set-offset 'inher-cont '++)
           (c-set-offset 'brace-list-intro '+)
           (c-set-offset 'func-decl-cont '++))
     (encoding . utf-8)))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch :background "MediumPurple4"))))
 '(vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

;; locale settings
(setenv "LC_ALL" "ja_JP.UTF-8")

(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)
(put 'set-goal-column 'disabled nil)
(add-hook 'text-mode-hook (lambda () (setq auto-fill-mode nil)))
(add-hook 'html-mode-hook (lambda () (setq auto-fill-mode nil)))

(cond ((equal system-type 'darwin)
       (setenv "GIT_PAGER" "")
       nil)
      ((equal system-type 'windows-nt)
       (use-package disable-mouse
         :ensure t
         :config
         (global-disable-mouse-mode)
         :init
         (put 'dired-find-alternate-file 'disabled nil)
         )
       )
      (t
       (defun my:ignore (&rest args)
         (interactive)
         (setq this-command last-command)
         nil)
       (global-set-key (kbd "<eisu-toggle>") 'my:ignore)

       ;; disable-mouse when linux environement
       (use-package disable-mouse
         :ensure t
         :config
         (global-disable-mouse-mode)
         :init
         (put 'dired-find-alternate-file 'disabled nil)
         )
       ))


