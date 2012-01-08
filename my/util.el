(defmacro aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2)
           (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro and-let* (bindings &rest body)
  "imported from srfi-2"
  (declare (indent 1))
  (reduce #'(lambda (binding r)
              (let ((head (car binding)))
                (cond ((and (atom head) (symbolp head))
                       `(let (,binding)
                          (when ,head ,r)))
                      ((listp head)
                       `(when ,head ,r))
                      (t
                       (error "and-let*: invalid head %s" head)))))
          bindings :from-end t :initial-value `(progn ,@body)))


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

;;;

(defmacro bind-region-or-line-with (&rest ac)
  `(multiple-value-bind (beg end)
       (if (region-active-p)
	   (values (region-beginning) (region-end))
	   (values (point-at-bol) (point-at-eol)))
     ,@ac))

(defmacro pp-mac* (macro)
  (require 'pp)
  `(let ((print-level nil)
         (print-length nil))
     (pp
      (cl-prettyexpand (quote ,macro)) (get-buffer (current-buffer)))))

(defmacro def-toggle (name &rest body)
  (and-let* ((on-clause (aif (assoc-default :on body) `(progn ,@it)))
	     (off-clause (aif (assoc-default :off body) `(progn ,@it)))
	     (state (gensym)) (flag (gensym)))
    `(lexical-let (,state)
       (defun ,name (&optional ,flag) (interactive "P")
	 (case ,flag
	   ((1 t) (progn ,on-clause (setq ,state t)))
	   ((-1) (progn ,off-clause (setq ,state nil)))
	   (otherwise (,name (if (not ,state) 1 -1))))))))


;;

(defun directory-files2 (directory &optional full nosort)
  (directory-files directory full "^[^\\.]\\{1,2\\}" nosort))

(defun decompose-file-path (path) ;;util
  (let ((ext  (file-name-extension path))
	(basename (file-name-nondirectory path)))
    (if ext
	(values (file-name-directory path)
		(substring basename 0 (string-match (format "\\.%s$" ext) basename))
		(concat "." ext))
	(values (file-name-directory path) basename ""))))

(defun check-target-is-exist-in-path (path target &optional find-all-p)
  (destructuring-bind (head . words) (split-string path"/")
    (let ((candidates
           (loop for word in words
                 with acc = head
                 unless (string-equal "" word)
                 do (setq acc (concat acc "/" word))
                 and collect acc into result
                 finally return (nreverse (cons head result)))))
        (cond (find-all-p
               (lexical-let ((target target))
                 (remove-if-not (lambda (path)
                                  (file-exists-p (concat path "/" target)))
                                candidates)))
              (t
               (find target candidates
                        :test (lambda (target path)
                                (file-exists-p (concat path "/" target)))))))))

(defun target-in-path (target &optional find-all-p)
  (and-let* ((dir (current-directory)))
    (check-target-is-exist-in-path
     (file-truename dir) target find-all-p)))
