(require 'cl)
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
      default-directory))

(defmacro named-progn (name &rest body)
  (declare (indent 1))
  `(progn ,@body))

(add-to-list 'load-path (current-directory))
(load "base")

;; 3rdparty
(add-to-list 'load-path (concat (current-directory) "3rdparty"))
(add-to-list 'load-path (concat (current-directory) "3rdparty/auto-complete-1.3.1"))

(require 'auto-complete nil t)
(global-auto-complete-mode t)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(require 'flymake)
(custom-set-faces
 '(flymake-errline ((((class color)) (:background "DarkRed"))))
 '(flymake-warnline ((((class color)) (:background "DarkBlue")))))

(named-progn python
  (defun py:setup-flymake-cmd (flymake-command)
    (cond ((executable-find flymake-command)
           (lexical-let
               ((flymake-command flymake-command))
             (defun py:flymake-pyflakes-init ()
               (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                  'flymake-create-temp-inplace))
                      (local-file          (file-relative-name
                                            temp-file 
                                            (file-name-directory buffer-file-name))))
                 (list flymake-command (list local-file)))))
           (add-to-list 'flymake-allowed-file-name-masks
                        '("\\.py$" py:flymake-pyflakes-init)))
          (t (message "%s is needed. (try pip install %s)"
                      flymake-command flymake-command))))

  (require 'python nil t) ;; old python
  (require 'python-mode nil t)

  ;; auto-complete
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
  (require 'ac-python nil t)

  ;; flymake
  (py:setup-flymake-cmd "pyflakes"))
