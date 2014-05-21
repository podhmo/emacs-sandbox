(progn ;; flymake 
  (require 'flymake)
  (require-and-fetch-if-not 'flymake-cursor)
  (eval-after-load 'flymake '(require 'flymake-cursor))
  (custom-set-variables
   '(help-at-pt-timer-delay 0.9)
   '(help-at-pt-display-when-idle '(flymake-overlay)))
  )

;; (progn ;; flycheck
;;   (require-and-fetch-if-not 'request)
;;   (require-and-fetch-if-not 'f)
;;   (require-and-fetch-if-not 'flycheck)
;;   (add-hook 'after-init-hook 'global-flycheck-mode)
;; ;  (flycheck-checkers)
;; ;  (flycheck-add-next-checker)
;;   )


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


(progn ;; auto-complete ;;move-it
  (require-and-fetch-if-not 'auto-complete)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  )


;; yasnippet
(require-and-fetch-if-not 'yasnippet)
(yas-load-directory "~/.emacs.d/snippets")

;; use "M-o" to expand, not "TAB"
(define-key yas-minor-mode-map (kbd "M-o") 'yas-expand)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; anything interface
(eval-after-load "anything-config"
  '(progn
     (defun my-yas/prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
              (selected (anything-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                         "*anything yas/prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
     (define-key anything-command-map (kbd "y") 'yas/insert-snippet)))

;; snippet-mode for *.yasnippet files
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
