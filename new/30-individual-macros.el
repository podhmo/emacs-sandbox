(progn ;; patch
  (require 'help-fns)

  (defun patch:function-arguments (sym)
    (read
     (car (help-split-fundoc (documentation sym t) sym))))
  )

(defmacro tapp (exp)
  (let ((tmp (gensym)))
    `(let ((,tmp ,exp))
       (print ,tmp)
       ,tmp)))

(progn ;; hashtable
  (defsubst hash-table-get (table k &optional default)
    (gethash k table default))

  (defsubst hash-table-put (table k v)
    (puthash k v table))

  (defun hash-table-keys (table)
    (loop for k being the hash-keys in table
          collect k))

  (defun hash-table-values (table)
    (loop for v being the hash-values in table
          collect v))

  (defun hash-table->alist (table)
    (loop for k being the hash-keys in table using (hash-value v)
          collect (cons k v)))

  (defun hash-table-mapcar (fn table)
    (loop for k being the hash-keys in table using (hash-value v)
          collect (fn k v))))

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

(defmacro ilambda (&rest body)
  `(lambda () (interactive) ,@body))

(defmacro with-lexical-bindings (syms &rest body)
  (declare (indent 1))
  (let ((clauses (loop for sym in syms collect (\` ((\, sym) (\, sym))))))
    (\` (lexical-let ((\,@ clauses)) (\,@ body)))))
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


(progn ;; enclose-element
  (defun enclose-element (beg-tag end-tag)
    (multiple-value-bind (beg end)
        (if (region-active-p)
            (values (region-beginning) (region-end))
          (values (progn (skip-syntax-backward "w_") (point))
                  (progn (skip-syntax-forward "w_") (point))))
      (save-excursion
        (let1 element (buffer-substring-no-properties beg end)
          (delete-region beg end)
          (insert (format "%s%s%s" beg-tag element end-tag))))))

  (defun enclose-element-interactive (tag) (interactive "s")
    (enclose-element tag tag)))

(progn ;; delete-syntax
  (defun* delete-syntax-forward (&optional (syntax "w_"))
    (delete-region (point) (progn (skip-syntax-forward syntax) (point))))
  
  (defun delete-syntax-forward* () (interactive)
    (if (looking-at-p "[ \t]")
        (delete-region (point) (progn (skip-chars-forward "[ \t]") (point)))
      (delete-syntax-forward)))
  )

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

