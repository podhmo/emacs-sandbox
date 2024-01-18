(eval-when-compile (require 'cl))
;;;; how to use
;; 
;; (define-insert-pair text-mode-map ("「" "」"))
;; (define-insert-pair text-mode-map ("{ -" "- }") :key "{")

;; (define-insert-pair* :key-maps (text-mode-map org-mode-map) :pair-list (("<" ">") ("（" "）")))
;; (define-insert-pair* :key-maps (text-mode-map) :pair-list (("( " " )" "(")))
;; (define-key c-mode-map "{" 'insert-{})

;;;
;;; internal-variable
;;;
(defvar insert-pair-store nil)

;;;
;;; internal-function
;;;

(defun insert-pair-push (key exp)
  (push (cons key exp) insert-pair-store))

(defvar insert-pair-in-escaped-p nil)
(defun insert-pair-escaped-after () (interactive)
  (setq insert-pair-in-escaped-p t)
  (insert "\\"))


(defun insert-pair-make-exp (beg-element end-element)
  (lexical-let ((beg-element beg-element)
                (end-element end-element))
    (lambda (n) (interactive "p")
      (let1 n (or n 1)
        (when (and insert-pair-in-escaped-p
                   (looking-back "\\\\"))
          (delete-backward-char 1))
        (dotimes (i n) (insert beg-element))
        (unless insert-pair-in-escaped-p
          (dotimes (i n) (insert end-element))
          (goto-char (- (point) (* (length end-element) n))))
        (setq insert-pair-in-escaped-p nil)))))

(defun insert-pair-get (beg-element)
  (cdr (assoc beg-element insert-pair-store)))

(defun* insert-pair-make (beg-element end-element &optional (key beg-element))
  (or (insert-pair-get key)
      (rlet1 exp (insert-pair-make-exp beg-element end-element)
        (insert-pair-push key exp))))

;;;
;;; for brace
;;;

(defun insert-{} () (interactive)
  (insert "{")
  (let1 key (read-key-sequence "type-key(RET or SPC):")
    (cond ((some (lambda (x) (string-equal key x))
                 '("
" "" "\t")) (insert-}-and-indent))
          ((some (lambda (x)  (string-equal key x))
                 " " "") (insert-}))
          (t (progn (insert-}) (insert key))))))

(defun insert-} ()  (insert "  }") (backward-char 2))
(defun insert-}-and-indent () 
  (insert "}")
  (backward-char 1)
  (newline-and-indent)
  (forward-line -1)
  (goto-char (point-at-eol))
  (newline-and-indent))

;;;
;;;functions for setting
;;;
(defun define-insert-pair-binding (key-map key-pairs)
  (loop for (l . r) in key-pairs
        if (atom r)
        do (define-key key-map l (insert-pair-make l r))
        else
        do (destructuring-bind (r key) r
             (define-key key-map key (insert-pair-make l r key)))))

(provide 'insert-pair-element)

