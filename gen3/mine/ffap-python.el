;;; ffap-python.el --- find-file-at-point of python. -*- lexical-binding: t; -*-
;; (defun ffap-python-setup ()
;;   (define-key python-mode-map (kbd "C-c C-f") 'ffap-python:import-ffap))
;; (add-hook 'python-mode-hook 'ffap-python-setup)

"
import foo
import foo.bar
import foo.bar, yah.yay #not supported
import foo.bar as b
from . import *
from foo import bar
from . import models #not supported
from foo import ( #not supported
  bar,
  boo
)
def f():
    import foo
    pass
"

(require 'cl)
(require 'pickup) ;; mine

;; custom variable (todo: use defcustom
(defvar ffap-python:python-program-name "python")
(defvar ffap-python:python-process-name "*ffap-python*")
(defvar ffap-python:output-buffer-name " *ffap-python*")
(defvar ffap-python:confirm-before-find-file-p t)


(defstruct %ffap-python:module-struct type module symbol)

;; todo: test
(defvar %ffap-python:module-tokens-regexp "\\(\\(?:[^ \n]+\\.?\\)+\\)")
(defvar %ffap-python:import-regexp-callback-pair-alist
  `((,(format "from %s import %s" %ffap-python:module-tokens-regexp %ffap-python:module-tokens-regexp)
     . (lambda () (make-%ffap-python:module-struct
                   :type 'import-symbol
                   :module (match-string-no-properties 1)
                   :symbol (match-string-no-properties 2))))
    (,(format "import %s" %ffap-python:module-tokens-regexp)
     . (lambda () (make-%ffap-python:module-struct
                   :type 'import-module
                   :module (match-string-no-properties 1)
                   :symbol nil)))))


;;; utils

;; create python code
(defun %ffap-python:translate-python-code-from-module (module)
  "get module path python code."
  ;; don't use "'" in python code!!
  (format "
import sys

try:
    import %s
    path = %s.__file__
    if path.endswith(\".pyc\"):
        path = path[:-1]
    sys.stdout.write(path)
except:
    sys.stdout.write(\"ERROR: %s file path is not found\")
" module module module))

(defun %ffap-python:translate-python-code-eval-stdin ()
  "import sys; exec(sys.stdin.read())")


;; buffer
(defun %ffap-python:get-fresh-buffer (bufname)
  (let ((buf (get-buffer bufname)))
    (cond (buf (with-current-buffer buf
                 (erase-buffer)
                 buf))
          (t (get-buffer-create bufname)))))

;; shell
(defun* %ffap-python:shell-command-buffer-async (procname buf cmd &key (callback 'display-buffer))
  (set-process-sentinel
   (start-process-shell-command procname buf cmd)
   (lexical-let ((callback callback))
     (lambda (process status)
       (let ((proc-buf (process-buffer process)))
         (run-with-timer 0.01 nil callback proc-buf))))))

(defun %ffap-python:join-path (xs ys)
  (let ((ys (if (string-match-p "^/" ys) (substring-no-properties ys 1) ys)))
    (cond ((string-match-p "/$" xs) (concat xs ys))
          (t (concat xs "/" ys)))))

;;; code
;; find-python
(defun ffap-python:find-python ()
  (ffap-python:find-program ffap-python:python-program-name))

(defun ffap-python:find-program (program)
  (let* ((venv-python-name
          (%ffap-python:join-path "bin" program)))
    (or (pickup-file venv-python-name)
        (executable-find program))))

;; find-symbol
(defun ffap-python:find-module-struct-maybe (&optional beg end)
  "[maybe] find module import sentence, beg and end are optional (default end value is `point-at-eol')"
  (let ((end (or end (point-at-eol)))
        (beg (or beg (point))))
    (save-excursion
      (loop for (regexp . if-matched-callback) in %ffap-python:import-regexp-callback-pair-alist
            do (goto-char beg)
            when beg do (goto-char beg)
            when (re-search-forward regexp end t 1)
            return (funcall if-matched-callback)))))

(defun ffap-python:module-struct-on-current-line ()
  "[maybe] return match object or nil"
  (save-excursion
    (ffap-python:find-module-struct-maybe
     (point-at-bol)
     (point-at-eol))))

;; eval
(defun ffap-python:eval-with-context-aware-python-async (python-code callback)
  (%ffap-python:shell-command-buffer-async
   ffap-python:python-process-name
   (%ffap-python:get-fresh-buffer ffap-python:output-buffer-name)
   (format "echo '%s' | %s -c '%s'"
           python-code
           (ffap-python:find-python)
           (%ffap-python:translate-python-code-eval-stdin))
   :callback callback
   ))


(defun ffap-python:find-file (filename &optional callback)
  (cond (ffap-python:confirm-before-find-file-p
         (and (or ffap-python-disable-confirm-before-open
                  (yes-or-no-p (format "open?(*ffap-python*): %s" filename)))
              (find-file filename)
              callback (funcall callback)))
        (t (find-file filename)
           (and callback (funcall callback)))))

;; ffap
(defun ffap-python:ffap-cont-default (buf mstruct)
  (let ((python-code-output
         (with-current-buffer buf
           (buffer-string))))
    (cond ((or (string-match-p "[a-zA-Z0-9_]ERROR:" python-code-output)
               (string-match-p "[a-zA-Z0-9_]Error:" python-code-output))
           (message python-code-output))
          (t
           (ffap-python:find-file
            python-code-output
            (lambda ()
              (when (%ffap-python:module-struct-symbol mstruct)
                (re-search-forward (%ffap-python:module-struct-symbol mstruct)
                                   nil t))))))))

(defvar ffap-python-ffap-cont 'ffap-python:ffap-cont-default)

(defun ffap-python:import-ffap-from-mstruct (mstruct)
  (lexical-let ((mstruct mstruct))
    (let ((python-code
           (%ffap-python:translate-python-code-from-module
            (%ffap-python:module-struct-module mstruct)))
          (callback
           (lambda (buf)
             (funcall ffap-python-ffap-cont
                      buf mstruct))))
      (ffap-python:eval-with-context-aware-python-async
       python-code
       callback))))

;; command

;;;###autoload
(defun ffap-python:import-ffap (&optional prefix-args) (interactive "P")
       (let ((mstruct
              (cond (prefix-args (make-%ffap-python:module-struct
                                  :type 'import-module
                                  :module (read-string "import module: ")
                                  :symbol nil))
                    (t (ffap-python:module-struct-on-current-line)))))
         (when mstruct
           (ffap-python:import-ffap-from-mstruct mstruct))))

(defvar ffap-python-disable-confirm-before-open nil)

(provide 'ffap-python)
;;; ffap-python.el ends here
