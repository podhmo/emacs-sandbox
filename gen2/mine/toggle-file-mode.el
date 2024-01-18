;;; toggle-file-mode.el --- toggle-file-mode. -*- lexical-binding: t; -*-
(require 'cl-lib)

(defvar toggle-file:default-rule nil)
(defvar toggle-file:rule-alist nil)

(define-minor-mode toggle-file-mode "toggle buffer via suffix"
  nil "<toggle>" '(("\C-c\C-c" . toggle-file)))

;; util
(defmacro suffilx-toggle:and-let*
    (bindings &rest body)
  "imported from srfi-2"
  (reduce (function
           (lambda (binding r)
             (let ((head (car binding)))
               (cond ((and (atom head) (symbolp head))
                      (\` (let ((\, binding)) (when (\, head) (\, r)))))
                     ((listp head)
                      (\` (when (\, head) (\, r))))
                     (t (error "and-let*: invalid head %s" head))))))
          bindings
          :from-end
          t
          :initial-value
          (\` (progn (\,@ body)))))
(put 'suffilx-toggle:and-let* 'lisp-indent-function 1)

(defun toggle-file:decompose-file-path (path)
  (let ((ext  (file-name-extension path))
        (basename (file-name-nondirectory path)))
    (cond (ext
           (values (file-name-directory path)
                   (substring basename 0 (string-match (format "\\.%s$" ext) basename))
                   (concat "." ext)))
          (t
           (values (file-name-directory path) basename "")))))

(defun toggle-file:default (file)
  (if (string-match "\\(.tmp\\|\\.tmp\\..+\\)$" file)
      (replace-regexp-in-string "\\.tmp" "" file)
    (multiple-value-bind (_ base ext) (toggle-file:decompose-file-path file)
      (concat base ".tmp" ext))))

(defun toggle-file:toggle-file (file)
  (or (toggle-file:match-rule file toggle-file:rule-alist)
      (if toggle-file:default-rule
          (funcall toggle-file:default-rule file)
        (toggle-file:default file))))

;;;###autoload
(defun toggle-file () (interactive)
       (let ((file (buffer-file-name)))
         (cond (file (find-file (toggle-file:toggle-file file))
                     (toggle-file-mode 1))
               (t (message "file: %s is not found" (buffer-name))))))

;; rule
(defun toggle-file:list-to-toggle-alist (seq)
  "'(a b c d e) -> '((a . b) (b . c) (c . d) (d . e) (e . a))
and if element of seq is regexp then strip '\\' and '$'"
  (cl-labels ((util-cons
            (x y)
            (cond ((stringp y) (cons x (replace-regexp-in-string "[\\^\\$]" "" y)))
                  (t (cons x y)))))
    (destructuring-bind (head . tail) seq
      `(,@(mapcar* 'util-cons seq tail)
        ,(util-cons (car (last tail)) head)))))

(defun toggle-file:add-toggle-rule! (seq)
  (add-to-list 'toggle-file:rule-alist
               (cons seq (toggle-file:list-to-toggle-alist seq))))

(defun toggle-file:match-rule (file rule-alist)
  ;; rule-alist is (((a b) ((a . b) (b . a))) ...)
  (suffilx-toggle:and-let*
      ((finder (lambda (rx-seq file)
                 (find file rx-seq :test (lambda (file rx) (string-match rx file)))))
       (rules (assoc-default file rule-alist finder)))
    (destructuring-bind (pat . rep)
        (assoc* file rules :test (lambda (file rx) (string-match rx file)))
      (replace-regexp-in-string pat rep file))))

;; convert-function?

;;;; Tests
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el

(dont-compile
  (when (fboundp 'expectations)
    (macrolet ((with-pre-toggle-alist
                (&rest actions)
                `(let ((---old---  toggle-file:rule-alist))
                   (unwind-protect
                       (progn ,@actions)
                     (setq toggle-file:rule-alist ---old---)))))
      (with-pre-toggle-alist
       (expectations
        (desc "list-to-toggle-alist")
        (expect '((a . b) (b . c) (c . d) (d . e) (e . a))
                (toggle-file:list-to-toggle-alist '(a b c d e)))
        (expect '(("\\.txt$" . ".rst") ("\\.rst$" . ".txt"))
                (toggle-file:list-to-toggle-alist '("\\.txt$" "\\.rst$")))
        (expect '(("^foo-" . "bar-") ("^bar-" . "foo-"))
                (toggle-file:list-to-toggle-alist '("^foo-" "^bar-")))

        (desc "decompose-file-path")
        (expect '("foo/" "bar" ".txt")
                (toggle-file:decompose-file-path "foo/bar.txt"))
        (expect '(nil "bar" ".txt")
                (toggle-file:decompose-file-path "bar.txt"))
        (expect '(nil "bar.tmp" ".txt")
                (toggle-file:decompose-file-path "bar.tmp.txt"))

        (desc "toggle-file:default")
        (expect "foo.tmp.txt"
                (toggle-file:default "foo.txt"))
        (expect "foo.txt"
                (toggle-file:default "foo.tmp.txt"))
        (expect "foo.0.1.2.tmp.txt"
                (toggle-file:default "foo.0.1.2.txt"))
        (expect "foo.0.1.2.txt"
                (toggle-file:default "foo.0.1.2.tmp.txt"))
        (expect ".emacs.tmp"
                (toggle-file:default ".emacs"))
        (expect ".emacs"
                (toggle-file:default ".emacs.tmp"))

        (desc "toggle-file")
        (expect "@"
                (let ((toggle-file:default-rule (lambda (x) "@")))
                  (toggle-file:toggle-file "foo.txt")))
        (expect "foo.tmp.txt"
                (toggle-file:toggle-file "foo.txt"))
        (expect "foo.txt"
                (toggle-file:toggle-file "foo.tmp.txt"))

        (desc "init") ;; init for after test
        (expect nil (progn (toggle-file:add-toggle-rule! '("\\.txt$" "\\.rst$")) nil))

        (desc "match-rule")
        (expect "foo.rst"
                (toggle-file:match-rule "foo.txt" toggle-file:rule-alist))
        (expect "foo.txt"
                (toggle-file:match-rule "foo.rst" toggle-file:rule-alist))

        (desc "toggle-file2")
        (expect "foo.rst"
                (toggle-file:toggle-file "foo.txt"))
        (expect "foo.txt"
                (toggle-file:toggle-file "foo.rst"))
        (expect "foo.tmp.dat"
                (toggle-file:toggle-file "foo.dat")) ;; note if foo.txt -> foo.rst (not foo.tmp.txt )
        (expect "foo.dat"
                (toggle-file:toggle-file "foo.tmp.dat")))))))
;;(expectations-execute)


;; test by hand
;;(setq toggle-file:rule-alist nil)
;;(toggle-file:add-toggle-rule! '("\\.el" "\\.rb"))
;; (toggle-file)
;;(toggle-file:add-toggle-rule! '("/toggle" "/prefix" "/infix" "/postfix"))
;; (toggle-file)

(provide 'toggle-file-mode)
