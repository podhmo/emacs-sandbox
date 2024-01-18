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
  (setq init-loader-show-log-after-init nil) ;; M-x init-loader-show-log

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
   '("824d07981667fd7d63488756b6d6a4036bae972d26337babf7b56df6e42f2bcd" "191bc4e53173f13e9b827272fa39be59b7295a0593b9f070deb6cb7c3745fd1d" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "d362eed16f74bfa8e49df0185a9336184d479e120c41837a5e6f020e0336bf7f" default))
 '(flycheck-check-syntax-automatically '(save mode-enabled))
 '(flycheck-command-wrapper-function #'my:flycheck-wrapper-function--use-fullpath-command)
 '(flycheck-executable-find #'my:flycheck-executable-find)
 '(flymake-no-changes-timeout nil)
 '(flymake-start-on-flymake-mode t)
 '(flymake-start-on-newline nil)
 '(flymake-start-on-save-buffer t)
 '(help-at-pt-display-when-idle '(flymake-overlay) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(package-selected-packages
   '(zen-mode solarized-theme typescript-mode which-key ivy-rich which-key-posframe magit ivy-postframe ivy-posframe jedi-core python-environment dracula-theme flycheck ivy counsel quickrun swiper paredit anything-match-plugin anything-config anything key-chord markdown-mode zlc go-eldoc elscreen bm init-loader eglot undo-tree jsonrpc json-rpc dash-functional lsp-mode shackle racer company-racer use-package company-jedi epc scala-mode disable-mouse flycheck-rust rust-mode go-mode fcitx "flymake-yaml" flymake-yaml yaml-mode toggle-file-mode pickup initchart flymake-jshint flymake-eslint ffap-python company-go anything-vcs-project))
 '(python-environment-virtualenv (list "python" "-m" "venv" "--system-site-packages"))
 '(safe-local-variable-values '((encoding . utf-8)))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "rosy brown" :overline "#8c8b85"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "Black"))))
 '(vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

;; locale settings
(setenv "LC_ALL" "ja_JP.UTF-8")

(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)
(put 'set-goal-column 'disabled nil)
(add-hook 'text-mode-hook (lambda () (setq auto-fill-mode nil)))
(add-hook 'html-mode-hook (lambda () (setq auto-fill-mode nil)))

(cond ((equal system-type 'darwin)
       (setq my:py-black-command "~/.local/bin/black")
       (use-package python
         :after (elscreen)
         :hook (python-mode . my:mac:python-mode-hook)
         :config
         (defun my:mac:python-mode-hook ()
           (define-key python-mode-map (kbd "C-c C-p") 'elscreen-previous)
           )
         ))
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


