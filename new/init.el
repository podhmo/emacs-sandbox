;;; Begin 0
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(add-to-list 'load-path (current-directory))

;;; begin 1
(add-to-list 'load-path (current-directory))
(add-to-list 'load-path (concat (current-directory) "3rdparty"))

(load "package+")

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

;; init-loader
(require-and-fetch-if-not 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load (current-directory))

;; code-block
(require-and-fetch-if-not 'config-block :url "https://gist.github.com/podhmo/3917706/raw/3bd74df6fbc69995be57f4635d4b14f2afeedaa4/config-block.el")

;; this is ad-hoc settings for mac
(define-key global-map "¥" (lambda (&optional n) (interactive "p") (dotimes (i (or n 1))  (insert "\\"))))
(setq-default tab-stop-list (loop for i from 4 to 120 by 4
                                  collect i))

(setq-default ring-bell-function
              (lambda () (message "ding")))

(put 'narrow-to-region 'disabled nil)
(setq debug-on-error nil)

;; (require-and-fetch-if-not 'elpy)
;; (elpy-enable)

;;; initialize
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
    )

(run-hook-with-args 'after-init-hook)

;; gist
;; (require 'gist)

(defun flymake-post-syntax-check (exit-status command)
  (setq flymake-err-info flymake-new-err-info)
  (setq flymake-new-err-info nil)
  (setq flymake-err-info
        (flymake-fix-line-numbers
         flymake-err-info 1 (flymake-count-lines)))
  (flymake-delete-own-overlays)
  (flymake-highlight-err-lines flymake-err-info)
  (let (err-count warn-count)
    (setq err-count (flymake-get-err-count flymake-err-info "e"))
    (setq warn-count  (flymake-get-err-count flymake-err-info "w"))
    (flymake-log 2 "%s: %d error(s), %d warning(s) in %.2f second(s)"
		 (buffer-name) err-count warn-count
		 (- (flymake-float-time) flymake-check-start-time))
    (setq flymake-check-start-time nil)

    (if (and (equal 0 err-count) (equal 0 warn-count))
	(if (equal 0 exit-status)
	    (flymake-report-status "" "")	; PASSED
	  (if (not flymake-check-was-interrupted)
	      ;; (flymake-report-fatal-status "CFGERR"
		  ;;   		   (format "Configuration error has occurred while running %s" command))
	    (flymake-report-status nil ""))) ; "STOPPED"
      (flymake-report-status (format "%d/%d" err-count warn-count) ""))))


;; hy
(require 'hy-mode)
;; (define-derived-mode hy-mode lisp-mode "hy"
;;   (add-hook 'hy-mode-hook 'turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.hy$" . hy-mode))

(add-to-list 'quickrun-file-alist '("\\.hy$" . "hy"))
(add-to-list 'quickrun/language-alist
             '("hy" . ((:command . "~/vboxshare/venvs/my3/bin/hy")
                       (:compile-only . "~/vboxshare/venvs/my3/bin/hyc %n.hy")
                       (:description "Run Hy script"))))
(put 'set-goal-column 'disabled nil)
