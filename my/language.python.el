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
  (python:define-plugin python:strict-indent-plugin (&optional (tabsize 4))
    (lexical-let ((tabsize tabsize))
      (python:with-plugin-mode-hook
       (setq-default indent-tabs-mode nil)
       (setq-default tab-width tabsize))))

  (python:define-plugin python:auto-mode-alist-plugin ()
    (add-to-list 'auto-mode-alist `("\\.p\\(yx\\|xd\\)$" . ,python:python-mode)))
  )
  (python:define-plugin python:autopair-plugin ()
    (require-and-fetch-if-not 'autopair)
    (python:with-plugin-mode-hook
     (autopair-on)))

  (python:define-plugin python:flymake-plugin (&optional (validate-program "pyflakes"))
    (require-and-fetch-if-not 'flymake)
    (setq python:flymake-program validate-program)

    (named-progn utility-for-flymake
      (defun* python:flymake-create-temp (file-name &optional (prefix "flymake-python"))
        (make-temp-file prefix nil ".py"))
    
      (defun python:flymake-init ()
        (let ((temp-file (flymake-init-create-temp-buffer-copy 
                          'python:flymake-create-temp)))
          (list (python:get-virtualenved python:flymake-program) (list temp-file)))))

    (named-progn treat-timer-as-active-timer-is-one
      (defun python:flymake-kill-timer ()
        (when flymake-timer
          (cancel-timer flymake-timer)
          (setq flymake-timer nil)))

      (defun python:flymake-kill-other-timer ()
        (dolist (b python:flymake-timered-buffers)
          (with-current-buffer b
            (python:flymake-kill-timer)))
        (setq python:flymake-timered-buffers nil))

      (defun python:flymake-rebirth-timer ()
        (unless flymake-timer
          (setq flymake-timer
                (run-at-time nil 1 'flymake-on-timer-event (current-buffer)))
          (push (current-buffer) python:flymake-timered-buffers)))

      (defvar python:flymake-timered-buffers nil)

      (named-progn utility
        (defsubst python:singlep (pair)
          (null (cdr pair)))
        (defsubst python:is-same-current-buffer-and-timered-buffer ()
          (and (python:singlep python:flymake-timered-buffers)
               (equal (current-buffer) (car python:flymake-timered-buffers)))))

      (named-progn advices
        (when (require 'elscreen nil t)
          (defadvice elscreen-goto (after kill-other-flymake-timer activate)
            (when (equal python:python-mode major-mode)
              (unless (python:is-same-current-buffer-and-timered-buffer)
                (python:flymake-kill-other-timer)
                (python:flymake-rebirth-timer)))))
        
        (defadvice switch-to-buffer (after kill-other-flymake-timer activate)
          (when (equal python:python-mode major-mode)
            (unless (python:is-same-current-buffer-and-timered-buffer)
              (python:flymake-kill-other-timer)
              (python:flymake-rebirth-timer)))))

    (named-progn add-hook
      (named-progn support-virtual-env ;;todo: move-it
        (defun python:flymake-program-real (program)
          (if (functionp program)
              (funcall program)
            program))
        
        (defalias 'python:target-in-path 'target-in-path) ;; import from my util.el
        
        (defun* python:get-virtualenved (cmd &optional (prefix "bin/"))
          (let ((venvroot (python:target-in-path (concat prefix cmd))))
            (if venvroot (concat venvroot "/" prefix cmd) cmd))))

        (python:with-plugin-mode-hook
         (set (make-local-variable 'flymake-allowed-file-name-masks) '(("." python:flymake-init)))
         ;; (set (make-local-variable 'flymake-err-line-patterns) flymake-python-err-line-patterns)
         (let ((flymake-program (python:get-virtualenved python:flymake-program)))
           (cond ((executable-find flymake-program)
                  (flymake-mode-on)
                  (python:flymake-kill-other-timer)
                  (push (current-buffer) python:flymake-timered-buffers))
                 (t
                  (message "Not enabling flymake: '%s' command not found" flymake-program))))))))

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
        ;; (message "dispatch")
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
