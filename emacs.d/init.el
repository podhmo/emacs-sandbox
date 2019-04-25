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
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flycheck-executable-find (function my:flycheck-executable-find))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(package-selected-packages
   (quote
    (jedi-core python-environment dracula-theme flycheck ivy counsel quickrun swiper paredit anything-match-plugin anything-config anything key-chord markdown-mode zlc go-eldoc elscreen bm init-loader eglot undo-tree jsonrpc json-rpc dash-functional lsp-mode shackle racer company-racer use-package company-jedi epc scala-mode disable-mouse flycheck-rust rust-mode go-mode fcitx "flymake-yaml" flymake-yaml yaml-mode toggle-file-mode pickup initchart flymake-jshint flymake-eslint ffap-python company-go anything-vcs-project)))
 '(python-environment-virtualenv (list "python" "-m" "venv" "--system-site-packages"))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch :background "MediumPurple4")))))

;; locale settings
(setenv "LC_ALL" "ja_JP.UTF-8")

(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)
(put 'set-goal-column 'disabled nil)
(add-hook 'text-mode-hook (lambda () (setq auto-fill-mode nil)))
(add-hook 'html-mode-hook (lambda () (setq auto-fill-mode nil)))

(unless (equal system-type 'darwin) ; mozc (japanese input)
  (defun my:ignore (&rest args)
    (interactive)
    (setq this-command last-command)
    nil)
  (global-set-key (kbd "<eisu-toggle>") 'my:ignore)

  ;; disable-mouse when linux environement
  (require 'disable-mouse)
  (global-disable-mouse-mode)
  )
(put 'dired-find-alternate-file 'disabled nil)
