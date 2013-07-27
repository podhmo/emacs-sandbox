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

