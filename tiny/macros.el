;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'cl-lib)

(defmacro let1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let ((,var ,val))
     ,@body))

(defmacro rlet1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let1 ,var ,val
     ,@body
     ,var))

(defmacro aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2)
           (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(unless (fboundp 'and-let*)
  (require 'subr-x) ; for emacs 27.1
  )


(defmacro def-toggle (name &rest body)
  (and-let* ((on-clause (aif (assoc-default :on body) `(progn ,@it)))
             (off-clause (aif (assoc-default :off body) `(progn ,@it)))
             (state (gensym)) (flag (gensym)))
    `(let (,state)
       (defun ,name (&optional ,flag) (interactive "P")
              (cl-case ,flag
		((1 t) (progn ,on-clause (setq ,state t)))
		((-1) (progn ,off-clause (setq ,state nil)))
		(otherwise (,name (if (not ,state) 1 -1))))))))
