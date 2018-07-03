(config-block! 
 nil 
 (defun addon:python.core ()
   (setq a:python-indent 4)
   (setq a:python-mode-hook 'python-mode-hook)
   ))

(config-block!
 '(addon:python.core)
 (defun addon:python.hook.initialize ()
   (add-hook a:python-mode-hook
             (defun a:setup.core ()
               (setq a:python-mode-map python-mode-map)
               )
             )
   ))

(config-block!
 '(addon:python.core)
 (defun addon:python.strict.indent ()
   (let ((tabsize a:python-indent))
     (setq-default indent-tabs-mode nil)
     (setq-default tab-width tabsize))
   ))

(config-block!
 '(addon:python.core)
 (defun addon:python.find-python ()
   (require-and-fetch-if-not 'python-import-utils :url "https://gist.github.com/podhmo/4517315/raw/38f29ed9a009841efd1b3356b30f8966e5ad9227/python-import-utils.el")
   (defalias 'find-python (symbol-function 'python-import-utils:find-python))
   ))

(config-block!
 '(addon:python.core addon:python.find-python)
 (defun addon:python.import.ffap ()
   (add-hook a:python-mode-hook
             (defun a:setup.ffap ()
               (define-key a:python-mode-map (kbd "C-c C-f") 'python-import-utils:import-ffap)
               ))))

(config-block!
 '(addon:python.core addon:python.find-python)
 (defun addon:python.quickrun ()
   (require-and-fetch-if-not 'quickrun)

   (defun python-virtualenv ()
     (concat (find-python) " -W default"))

   ;; remove python setting
   (setq quickrun/language-alist 
         (remove* "python" quickrun/language-alist :key 'car :test 'equal))
   (push '("python" . ((:command . python/virtualenv) (:compile-only . "pyflakes %s")
                       (:description . "Run Python script")))
         quickrun/language-alist)
   ))


