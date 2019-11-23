(use-package flymake
  :ensure t
  :config
  ;; renewal version (1.0.x)
  (assert (<= 1 (car (pkg-info-package-version 'flymake))))

  ;; :emergency :error :warning :debug
  ;; (setq warning-minimum-log-level :warning)

  (custom-set-variables
   '(flymake-start-on-newline nil) ;; for auto-save-buffers.el
   '(flymake-start-on-save-buffer t)
   '(flymake-start-on-flymake-mode t)
   '(flymake-no-changes-timeout nil) ;; for performance
   )
  )

(use-package flycheck
  :ensure t
  :commands (flycheck-mode flycheck-mode-on-safe)
  :config

  ;; NOTE:
  ;; after new checker is installed,
  ;; M-x flycheck-verify-setup, flycheck-verify-checker

  ;; commandを探す方法をカスタマイズ
  (defvar my:flycheck-executable-find-function-table (make-hash-table :test 'equal :size 7))
  (defun my:flycheck-executable-find-function-register (name thunk)
    (puthash name thunk my:flycheck-executable-find-function-table))

  (defun my:flycheck-executable-find (command)
    (let ((find-function
           (gethash command my:flycheck-executable-find-function-table)))
      (cond (find-function (executable-find (funcall find-function)))
            (t (executable-find command)))))

  (setq flycheck-hooks-alist
        (delq (assoc 'after-change-functions flycheck-hooks-alist)
              flycheck-hooks-alist))

  ;; call command with full path
  (defun my:flycheck-wrapper-function--use-fullpath-command (command)
    ;; command :: (<cmd name> ,@args)
    (cons (funcall flycheck-executable-find (car command))
          (cdr command))
    )

  (custom-set-variables
   '(flycheck-check-syntax-automatically '(save mode-enabled)) ;; for-performance
   '(flycheck-executable-find #'my:flycheck-executable-find) ; risky
   '(flycheck-command-wrapper-function #'my:flycheck-wrapper-function--use-fullpath-command) ; risky
   )
  )


(use-package quickrun
  :ensure t
  :commands
  (quickrun quickrun-compile-only)
  :config
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

(progn ;; company
  (require-and-fetch-if-not 'company)
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")
  (global-set-key (kbd "C-M-i") 'company-complete)

  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

  ;; TABで候補を設定
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)
  )

;; formatterを現在のbufferに適用するための関数群
(defun my:get-fresh-buffer-create (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setf (buffer-string) ""))
    buf
    ))

(defun my:execute-formatter-command (cmd-name cmd beg end &optional force)
  (cond ((executable-find cmd-name)
         (let ((p (point)))
           (save-excursion
             (save-restriction
               (narrow-to-region beg end)
               (let ((buf (my:get-fresh-buffer-create (format "*%s*" cmd-name)))
                     (err-buf (my:get-fresh-buffer-create (format "*%s error*" cmd-name))))
                 (let ((status
                        ;; xxx
                        (flet ((display-message-or-buffer (&rest args) nil))
                          (shell-command-on-region (point-min) (point-max) cmd buf nil err-buf)
                          )))
                   (cond ((= 0 status)
                          (let ((replaced (with-current-buffer buf (buffer-string))))
                            (cond ((string= replaced "")
                                   (message "succeeded with no output"))
                                  (t
                                   (delete-region (point-min) (point-max))
                                   (insert replaced)
                                   ))))
                         (t (message (with-current-buffer err-buf (buffer-string)))))))))
           (when (= (point) (point-min)) (goto-char p))))
        (t (message (format "%s is not found" cmd-name)))))
