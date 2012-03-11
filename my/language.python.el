(progn ;; prepare
  (unless (fboundp 'require-and-fetch-if-not)
    (defalias 'require-and-fetch-if-not 'require))
  (unless (fboundp 'named-progn)
    (defmacro named-progn (name &rest body)
      (declare (indent 1))
      `(progn ,@body))))

(named-progn define-internal-variables
  (defvar python:plugin-mode-hook '())
  (defvar python:python-mode 'python-mode)
  (defvar python:activated-plugins '())
  )

(named-progn define-utility
  (defmacro python:with-plugin-mode-hook (&rest body)
    `(add-hook 'python:plugin-mode-hook
               (lambda ()
                 ,@body)))

  (defmacro python:define-plugin (name args &rest body)
    (declare (indent 2))
    `(defun* ,name ,args 
       (add-to-list 'python:activated-plugins ',name)
       ,@body)))

(named-progn plugins-are-here
  (python:define-plugin python:autopair-plugin ()
    (require-and-fetch-if-not 'autopair)
    (python:with-plugin-mode-hook
     (autopair-on)))


  (python:define-plugin python:flymake-plugin (&optional (validate-program "pyflakes"))
    (require-and-fetch-if-not 'flymake)
    (setq python:flymake-program validate-program)

    (defun python:flymake-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (cmd (format "%s %s" python:flymake-program temp-file)))
        (list shell-file-name 
              (list shell-command-switch cmd)
              default-directory)))

    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.py\\'" python:flymake-init))
    (python:with-plugin-mode-hook
     (flymake-mode-on)))

  (python:define-plugin python:flymake-eldoc/current-position-plugin ()
    (require 'eldoc nil t)
    (named-progn define-variables-and-function
      (defvar python:flymake-eldoc-timer nil)
      (defvar python:flymake-eldoc-timer-delay 0.25)

      (defun python:flymake-eldoc-print-current-message-info ()
        (and-let* ((err (get-char-property (point) 'help-echo)))
          (eldoc-message "flymake-eldoc: %s" err)))

      (defun python:flymake-eldoc-cancel () (interactive)
        (cancel-timer python:flymake-eldoc-timer)
        (setq python:flymake-eldoc-timer nil))
      
      (defun python:flymake-eldoc-dispatch ()
        (message "dispatch")
        (cond ((equal major-mode python:python-mode)
               (python:flymake-eldoc-start))
              (python:flymake-eldoc-timer
               (python:flymake-eldoc-cancel))))

      (defun python:flymake-eldoc-start ()
        (unless python:flymake-eldoc-timer
          (setq python:flymake-eldoc-timer
                (run-with-idle-timer python:flymake-eldoc-timer-delay t
                                     'python:flymake-eldoc-print-current-message-info)))))

    (named-progn add-hook
      (python:with-plugin-mode-hook
       (named-progn dependency-check
         (unless (member 'python:flymake-plugin python:activated-plugins)
           (error "this plugin depends on `python:flymake-plugin"))
         (unless (boundp 'cmh:change-mode-hook)
           (error "this plugin depends on `cmh:change-mode-hook (individual package)")))

       (add-hook 'cmh:change-mode-hook 'python:flymake-eldoc-dispatch)
       (python:flymake-eldoc-start))))

  (python:define-plugin python:strict-indent-plugin (&optional (tabsize 4))
    (lexical-let ((tabsize tabsize))
      (python:with-plugin-mode-hook
       (setq-default indent-tabs-mode nil)
       (setq-default tab-width tabsize))))

  (python:define-plugin python:auto-mode-alist-plugin ()
    (add-to-list 'auto-mode-alist `("\\.p\\(yx\\|xd\\)$" . ,python:python-mode)))
  )