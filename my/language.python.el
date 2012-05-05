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
       ,@body))

  (defun python:plugin-activate-p (plugin)
    (memq plugin python:activated-plugins)))

(named-progn plugins-are-here
  (python:define-plugin python:yasnippet-plugin ()
    (python:with-plugin-mode-hook
     (yas/minor-mode-on)))

  (python:define-plugin python:strict-indent-plugin (&optional (tabsize 4))
    (lexical-let ((tabsize tabsize))
      (python:with-plugin-mode-hook
       (setq-default indent-tabs-mode nil)
       (setq-default tab-width tabsize))))

  (python:define-plugin python:run-program-plugin-simple ()
    (defun python:run-program-current-buffer () (interactive)
      (and-let* ((fname (buffer-file-name)))
        (compile
         (format "%s %s" 
                 (python:get-virtualenved "python")
                 fname)))))

  (python:define-plugin python:auto-mode-alist-plugin ()
    (add-to-list 'auto-mode-alist `("\\.p\\(yx\\|xd\\)$" . ,python:python-mode)))

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
        ;; (when (require 'elscreen nil t)
        ;;   (defadvice elscreen-goto (after kill-other-flymake-timer activate)
        ;;     (when (equal python:python-mode major-mode)
        ;;       (unless (python:is-same-current-buffer-and-timered-buffer)
        ;;         (python:flymake-kill-other-timer)
        ;;         (python:flymake-rebirth-timer)))))

        (defadvice switch-to-buffer (after kill-other-flymake-timer activate)
          (when (equal python:python-mode major-mode)
            (unless (python:is-same-current-buffer-and-timered-buffer)
              (python:flymake-kill-other-timer)
              (python:flymake-rebirth-timer)))))

      (named-progn add-hook
        (named-progn support-virtual-env ;;todo: move-it
          (defalias 'python:target-in-path 'target-in-path) ;; import from my util.el
          
          (defun* python:get-virtualenved (cmd &optional (prefix "bin/"))
            (let ((venvroot (python:target-in-path (concat prefix cmd))))
              (if venvroot (concat venvroot "/" prefix cmd) cmd))))

        (python:with-plugin-mode-hook
         (let ((flymake-program (python:get-virtualenved python:flymake-program)))
           (cond ((executable-find flymake-program)
                  (set (make-local-variable 'flymake-allowed-file-name-masks) '(("." python:flymake-init)))
                  ;; (set (make-local-variable 'flymake-err-line-patterns) flymake-python-err-line-patterns)
                  (flymake-mode-on)
                  (python:flymake-kill-other-timer)
                  (push (current-buffer) python:flymake-timered-buffers))
                 (t
                  (message "Not enabling flymake: '%s' command not found" flymake-program))))))))

  (python:define-plugin python:anything-with-modules-plugin ()
    (unless (require 'anything nil t)
      (error "anything is not found"))
    ;; todo refactoring

    (setq python:module-tokens-regexp "[^\n .]+\\(\\.[^\n .]+\\)*")
    (setq python:module-import-sentence-regexp 
          (format "^[\n \t]*\\(from +\\(%s\\) +import +[^ \n]+\\|import +\\(%s\\)\\)"
                  python:module-tokens-regexp
                  python:module-tokens-regexp))

    (defun* pyutil:find-file-safe (path &key open)
      (and (file-exists-p path)
           (file-readable-p path)
           (funcall (or open 'find-file) path)))

    (defun pyutil:shell-command-to-string* (command)
      (let ((r (shell-command-to-string command)))
        (substring-no-properties r 0 (- (length r) 1))))
    
    (defun pyutil:fresh-buffer-and-is-created (buf &optional force-erase-p)
      (cond ((and (stringp buf) (not (get-buffer buf)))
             (values (get-buffer-create buf) t))
            (t
             (let1 buf (if (bufferp buf) buf (get-buffer buf))
               (when force-erase-p
                 (with-current-buffer buf
                   (erase-buffer)))
               (values buf force-erase-p)))))

    (defun pyutil:command-to-buffer-ansync (procname bufname cmd &optional force-reload-p call-back)
      (let1 call-back (or call-back 'display-buffer)
        (multiple-value-bind (buf new-p)
            (pyutil:fresh-buffer-and-is-created bufname force-reload-p)
          (cond (new-p
                 (set-process-sentinel
                  (start-process-shell-command procname buf cmd)
                  (with-lexical-bindings (call-back)
                    (lambda (process status)
                      (funcall call-back (process-buffer process))))))
                (t (funcall call-back (get-buffer bufname))))
          buf)))

    (lexical-let ((previous-python nil))
      (defun python:all-modules-to-buffer (&optional force-reload-p call-back)
        ;; return buffer
        (let1 python (python/virtualenv)
          (let* ((reload-p (or force-reload-p (not (string-equal previous-python python))))
                 (cmd (format "%s -c 'import pydoc; import sys; pydoc.ModuleScanner().run(lambda path,modname,desc : sys.stdout.write(modname+\"\\n\"))' 2>/dev/null | sort -u" python))
                 (bufname "*python all-modules*"))
            (setq previous-python python)
            (pyutil:command-to-buffer-ansync "python-ex-util:all-module"
                                             bufname cmd reload-p call-back)))))

    (defun python:find-module-tokens-maybe (&optional beg end)
      "[maybe] find module import sentence, beg and end are optional (default end value is `point-at-eol')"
      (when beg
        (goto-char beg))
      (let ((end (or end (point-at-eol))))
        (and (re-search-forward python:module-import-sentence-regexp end t 1)
             (or (match-string-no-properties 4)
                 (match-string-no-properties 2)))))

    (defun python:module-tokens-in-current-line ()
      "[maybe] return match object or nil"
      (save-excursion
        (python:find-module-tokens-maybe (point-at-bol) (point-at-eol))))

    (defun python:library-path-list ()
      (let* ((script "import sys; D=[d for d in sys.path if not 'bin' in d]; print ','.join(D)")
             (sys-paths-str (pyutil:shell-command-to-string*
                             (format "%s -c \"%s\"" (python/virtualenv) script))))
        (cdr (split-string sys-paths-str ","))))
    
    (defun python:current-library-path ()
      "[maybe] return current library path from import sentence"
      (python:and-let* ((module (python:module-tokens-in-current-line)))
                       (python:module-name-to-file-path module)))
    
    (defun python:ffap/import-sentence (other-frame-p) (interactive "P")
      "ffap for import sentence"
      (python:and-let* ((path (python:current-library-path)))
                       (cond (other-frame-p (find-file-other-frame path))
                             (t (find-file path)))))

    (defun python:module-tokens-in-current-line ()
      "[maybe] return match object or nil"
      (save-excursion
        (python:find-module-tokens-maybe (point-at-bol) (point-at-eol))))

    (defvar python:collect-imported-modules-max 200)
    (defun python:collect-imported-modules-in-buffer (&optional buf)
      (let1 buf (or buf (current-buffer))
        (with-current-buffer buf
          (goto-char (point-min))
          (let1 end (min (point-max) python:collect-imported-modules-max) ;;
            (loop while t
                  for token = (python:find-module-tokens-maybe nil end)
                  if token
                  collect token into modules
                  else
                  return (delete-duplicates modules :test 'string-equal))))))

    (defun python:anything-update-callback (buffer)
      (anything-update)
      buffer)

    (define-anything-type-attribute 'python-module
      '((action . (("find-file" . 
                    (lambda (c)
                      (and-let* ((path (python:module-name-to-file-path c)))
                        (pyutil:find-file-safe path))))
                   ("find-file-other-frame" .
                    (lambda (c)
                      (and-let* ((path (python:module-name-to-file-path c)))
                        (pyutil:find-file-safe path :open 'find-file-other-frame))))
                   ("web-page" .
                    (lambda (c)
                      (and-let* ((url (python:module-name-to-web-page c)))
                        (message "browse: %s" url)
                        (browse-url url))))
                   ("info-egg" .
                    (lambda (c)
                      (and-let* ((path (python:module-name-to-egg-info c)))
                        (pyutil:find-file-safe path)))))))
      "Python module")

    (setq python:anything-c-source-imported-modules
          '((name . "imported modules")
            (candidates . (lambda ()
                            (python:collect-imported-modules-in-buffer  anything-current-buffer)))
            (update . python:all-modules-to-buffer-reload)
            (type . python-module)))

    (setq python:anything-c-source-all-modules
          '((name . "python all module")
            (candidates-in-buffer)
            (init . (lambda () 
                      (anything-candidate-buffer
                       (python:all-modules-to-buffer nil 'python:anything-update-callback))))
            (update .  python:all-modules-to-buffer-reload)
            (type . python-module)))

    (defvar python:standard-doc-url-base "http://docs.python.org/library")

    (defun python:module-name-to-file-path (module &optional force-reload-p) ;;force-reload is not implemented
      "python module -> file path"
      (let1 path (replace-regexp-in-string "\\." "/" module)
        (loop for dir in (python:library-path-list)
              for d = (concat dir "/" path)
              if (file-exists-p d)
              return d
              else
              for file = (concat d ".py")
              when (file-exists-p file)
              return file)))

    (defun python:module-name-to-egg-info (module &optional force-reload-p) ;;force-reload is not implemented
      (let* ((module-top (car (split-string module "\\.")))
             (info-rx (format "\\(%s\\|%s\\)-.*egg-info$" (capitalize module-top) module-top)))
        (loop for dir in (python:library-path-list)
              when (and (file-exists-p dir) (file-directory-p dir))
              for candidate = (directory-files dir t info-rx t)
              when candidate return (car candidate))))
    
    (defun python:module-name-to-web-page (module &optional force-reload-p) ;;force-reload is not implemented
      (or (and-let* ((egg-info (python:module-name-to-egg-info module force-reload-p))
                     (file (format "%s/PKG-INFO" egg-info)))
            (with-current-buffer (find-file-noselect file)
              (goto-char (point-min))
              (re-search-forward "Home-page: *\\(.+\\)" nil t 1)
              (match-string 1)))
          (format "%s/%s.html" python:standard-doc-url-base module)))

    (defun python:anything-with-modules () (interactive)
      (let1 sources (list python:anything-c-source-imported-modules
                          anything-c-source-imenu
                          ;; python:anything-c-source-active-enves
                          python:anything-c-source-all-modules)
        (anything-other-buffer sources (get-buffer-create " *with-modules:python*"))))
    )

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

  (python:define-plugin python:quickrun-with-virtualenv-plugin ()
    (unless (require 'quickrun nil t)
      (error "this plugin `python:quickrun-plugin' require `quickrun', so please install it."))

    (defun python/virtualenv ()
      (python:get-virtualenved "python"))

    ;; remove python setting
    (named-progn setup
      (setq quickrun/language-alist 
            (remove* "python" quickrun/language-alist :key 'car :test 'equal))
      (push '("python" . ((:command . python/virtualenv) (:compile-only . "pyflakes %s")
                          (:description . "Run Python script")))
            quickrun/language-alist)
      ))
  (python:define-plugin python:swap-backquote-and-underscore-plugin ()
    (python:with-plugin-mode-hook

     (define-key (current-local-map) "`" (lambda () (interactive) (insert "_"))) ;; slack-off
     (define-key (current-local-map) "_" (lambda () (interactive) (insert "`")))))

  (python:define-plugin python:mako-html-plugin ()
    (add-to-list 'auto-mode-alist '("\\.mako?$" . html-mode))
    )

  (python:define-plugin python:auto-complete-plugin ()
    (unless (require 'auto-complete nil t)
      (error "this plugin require auto-complete-mode.el"))

    (defun ac-get-python-symbol-at-point ()
      "Return python symbol at point.Assumes symbol can be alphanumeric, `.' or `_'."
      (let ((end (point))
            (start (ac-python-start-of-expression)))
        (buffer-substring-no-properties start end)))
    
    (defun ac-python-completion-at-point ()
      "Returns a possibly empty list of completions for the symbol at point."
      (python-symbol-completions (ac-get-python-symbol-at-point)))

    (defun ac-python-start-of-expression ()
      "Return point of the start of python expression at point. Assumes symbol can be alphanumeric, `.' or `_'."
      (save-excursion
        (and (re-search-backward
              (rx (or buffer-start (regexp "[^[:alnum:]._]"))
                  (group (1+ (regexp "[[:alnum:]._]"))) point)
              nil t)
             (match-beginning 1))))

    (defvar ac-source-python
      '((candidates . ac-python-completion-at-point)
        (prefix . ac-python-start-of-expression)
        (symbol . "f")
        (requires . 2))
      "Source for python completion.")

    (named-progn setup
      (python:with-plugin-mode-hook
       (auto-complete-mode t)
       (add-to-list 'ac-sources 'ac-source-python))))
  )