(progn ;; flymake 
  (require 'flymake)
  ;; (require-and-fetch-if-not 'flymake-cursor)
  ;; (eval-after-load 'flymake '(require 'flymake-cursor))
  (custom-set-variables
   '(help-at-pt-timer-delay 0.9)
   '(help-at-pt-display-when-idle '(flymake-overlay)))
  )

(progn ;; flycheck
  (autoload 'flycheck-mode "flycheck" t)

  ;; commandを探す方法をカスタマイズ
  (defvar my:flycheck-executable-find-function-table (make-hash-table :test 'equal :size 7))
  (defun my:flycheck-executable-find-function-register (name thunk)
    (puthash name thunk my:flycheck-executable-find-function-table))

  (defun my:flycheck-executable-find (command)
    (let ((find-function
           (gethash command my:flycheck-executable-find-function-table)))
      (cond (find-function (executable-find (funcall find-function)))
            (t (executable-find command)))))

  (with-eval-after-load 'flycheck
    (custom-set-variables
     '(flycheck-check-syntax-automatically '(save mode-enabled))
     '(flycheck-executable-find #'my:flycheck-executable-find) ; risky
     )
    ))

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

;; 
;; (progn ;; auto-complete ;;move-it
;;   (require 'auto-complete)
;;   (setq ac-use-menu-map t)
;;   (define-key ac-menu-map "\C-n" 'ac-next)
;;   (define-key ac-menu-map "\C-p" 'ac-previous)
;;   )


;; yasnippet
;; (progn
;;   (require 'yasnippet)
;;   (yas-load-directory "~/.emacs.d/snippets")

;;   use "M-o" to expand, not "TAB"
;;   (define-key yas-minor-mode-map (kbd "M-o") 'yas-expand)
;;   (define-key yas-minor-mode-map (kbd "TAB") nil)

;;   ;; anything interface
;;   (eval-after-load "anything-config"
;;     '(progn
;;        (defun my-yas/prompt (prompt choices &optional display-fn)
;;          (let* ((names (loop for choice in choices
;;                              collect (or (and display-fn (funcall display-fn choice))
;;                                          choice)))
;;                 (selected (anything-other-buffer
;;                            `(((name . ,(format "%s" prompt))
;;                               (candidates . names)
;;                               (action . (("Insert snippet" . (lambda (arg) arg))))))
;;                            "*anything yas/prompt*")))
;;            (if selected
;;                (let ((n (position selected names :test 'equal)))
;;                  (nth n choices))
;;              (signal 'quit "user quit!"))))
;;        (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
;;        (define-key anything-command-map (kbd "y") 'yas/insert-snippet)))

;;   ;; snippet-mode for *.yasnippet files
;;   (add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
;;   )
