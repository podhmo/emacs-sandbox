;;; debug utiltities
(defvar cmh:debug-buffer nil)
(defvar cmh:me nil)
(defvar cmh:debug-buffer-name "*cmh:debug*")
(defvar cmh:is-display-when-debug-log t)
(defmacro cmh:with-debug-buffer (&rest body)
  `(progn
     (unless cmh:debug-buffer
       (setq cmh:debug-buffer (get-buffer-create cmh:debug-buffer-name)))
     (with-current-buffer cmh:debug-buffer
       ,@body)))

(defun cmh:debug-message-default ()
  (let1 time (format-time-string "/%Y/%m/%d %H:%M:%S" (current-time))
    (format "%s mode: (cmh:%s, %s) buffer:(%s)\n"
            time cmh:current-major-mode major-mode
            (current-buffer))))

(defvar cmh:debug-message-function 'cmh:debug-message-default)

(defun* cmh:debug-current-state (&optional (prefix "") (suffix ""))
  (let1 message (funcall cmh:debug-message-function)
    (cmh:with-debug-buffer
     (goto-char (point-max))
     (insert prefix message suffix)))
  (when cmh:is-display-when-debug-log
    (display-buffer cmh:debug-buffer)))

(defun cmh:debug-current-state* ()
  (cmh:debug-current-state (concat cmh:me "\t")))

(defun change-mode-hook-debug-activate () (interactive)
  (add-hook 'cmh:change-mode-hook 'cmh:debug-current-state*))

(defun change-mode-hook-debug-deactivate () (interactive)
  (remove-hook 'cmh:change-mode-hook 'cmh:debug-current-state*))

;;; code:
(defvar cmh:change-mode-hook nil)
(defvar cmh:current-major-mode nil)
;; (defvar cmh:exclude-major-mode-list '(lisp-interaction-mode))

(defadvice other-window (around cmh:hook-other-window activate)
  (setq cmh:me "other-window")
  ad-do-it
  (unless (eq cmh:current-major-mode major-mode)
    (run-hooks 'cmh:change-mode-hook)
    (setq cmh:current-major-mode major-mode)))

;;; switch-to-buffer and set-window-configuration are too noizy.
;; (defadvice switch-to-buffer (around cmh:hook-switch-to-buffer activate)
;;   (setq cmh:me "switch-to-buffer")
;;   (setq cmh:current-major-mode major-mode)
;;   ad-do-it
;;   (unless (eq cmh:current-major-mode major-mode)
;;     (run-hooks 'cmh:change-mode-hook)
;;     (setq cmh:current-major-mode major-mode)))
;; (defadvice set-window-configuration (around cmh:hook activate)
;;   (when (and (equal chm:current-buffer (current-buffer))
;;              (not (eq cmh:current-major-mode major-mode)))
;;     (run-hooks 'cmh:before-change-mode-hook))
;;   ad-do-it
;;   (when (and (equal chm:current-buffer (current-buffer))
;;              (not (eq cmh:current-major-mode major-mode)))
;;     (run-hooks 'cmh:before-change-mode-hook)
;;     (setq cmh:current-major-mode major-mode)))

(defadvice find-file (around cmh:hook-find-file activate)
  (setq cmh:me "find-file")
  (setq cmh:current-major-mode major-mode)
  ad-do-it
  (unless (eq cmh:current-major-mode major-mode)
    (run-hooks 'cmh:change-mode-hook)
    (setq cmh:current-major-mode major-mode)))

(when (fboundp 'elscreen-goto)
  (defadvice elscreen-goto (around cmh:hook-elscreen-goto activate)
    (setq cmh:me "elscreen-goto")
    (setq cmh:current-major-mode major-mode)
    ad-do-it
    (unless (eq cmh:current-major-mode major-mode)
      (run-hooks 'cmh:change-mode-hook)
      (setq cmh:current-major-mode major-mode))))

(require-and-fetch-if-not 'anything)

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

(defadvice anything-M-x (before keep-history-size-more-than-one activate)
  (while (null (cdr extended-command-history))
    (push "*dummy*" extended-command-history)))
