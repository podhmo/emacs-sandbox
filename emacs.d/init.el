(require 'cl)
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

  (progn
    ;; get available packages
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
   ;; (package-list-packages)
    )) 


(load (concat (current-directory) "init-visualize")) ;; hmm
;(initchart-visualize-init-sequence "/tmp/b.svg")

;;; Begin 1
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
;; suppress Warinig
(setq *curdir* (current-directory))
(defadvice init-loader-load (after suppress-warning activate)
  (setq load-path
        (remove-if (lambda (x) (string-equal x *curdir*)) load-path))
  )
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

;; temporary
(defun rst-htmlize () (interactive)
       (when (executable-find "htmlize")
         (shell-command (format "htmlize -b -t 1 %s" (buffer-file-name)))))

(eval-after-load "rst"
  '(add-hook 'rst-mode-hook
             (lambda () (define-key rst-mode-map "\C-c\C-c\C-l" 'rst-htmlize))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(package-selected-packages
   (quote
    (flycheck-rust rust-mode mozc go-mode fcitx "flymake-yaml" flymake-yaml yaml-mode toggle-file-mode py-yapf pickup initchart flymake-jshint flymake-eslint ffap-python company-go anything-vcs-project)))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; locale settings
(setenv "LC_ALL" "ja_JP.UTF-8")

(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x -") 'text-scale-decrease)
(put 'set-goal-column 'disabled nil)
(add-hook 'text-mode-hook (lambda () (setq auto-fill-mode nil)))
(add-hook 'html-mode-hook (lambda () (setq auto-fill-mode nil)))

(defun browse-github ()
  (interactive)
  (let ((replace-alist `((,(format "%s/src/github.com/\\([^/]+/[^/]+\\)/\\(.+\\)" (get-go-path)) . "https://github.com/\\1/tree/master/\\2"))))
    (let ((path buffer-file-name))
      (loop for (pat . rep) in replace-alist
            do (setq path (replace-regexp-in-string pat rep path)))
      (browse-url (format "%s#L%d" path (line-number-at-pos (point))))
      )))
(defun browse-github-master ()
  (interactive)
  (let ((replace-alist `((,(format "%s/src/github.com/\\([^/]+/[^/]+\\)/\\(.+\\)" (get-go-path)) . "https://github.com/\\1/tree/master/\\2"))))
    (let ((path buffer-file-name))
      (loop for (pat . rep) in replace-alist
            do (setq path (replace-regexp-in-string pat rep path)))
      (browse-url (format "%s#L%d" path (line-number-at-pos (point))))
      )))

(progn ; mozc (japanese input)
  (require 'mozc)
  (defun advice:mozc-key-event-with-ctrl-key--with-ctrl (r)
    (cond ((and (not (null (cdr r))) (eq (cadr r) 'control) (null (cddr r)))
           (case (car r)
             (102 r) ; C-f
             (98 r) ; C-b
             (110 '(down)) ; C-n
             (112 '(up))  ; C-p
             (t r)
             ))
          (t
           (case (car r)
             (kana '(enter))
             (henkan '(enter))
             (muhenkan '(backspace))
             (t r)
             ))))

 (advice-add 'mozc-key-event-to-key-and-modifiers :filter-return 'advice:mozc-key-event-with-ctrl-key--with-ctrl)
  (setq default-input-method 'japanese-mozc)
  )

