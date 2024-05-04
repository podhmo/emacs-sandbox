;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'cl-lib)

(defmacro aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2)
           (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))


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
