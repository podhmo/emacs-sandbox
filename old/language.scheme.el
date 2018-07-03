
(add-hook 'inferior-scheme-mode-hook
	  (lambda () (ansi-color-for-comint-mode-on)))

 (defun scheme-insert-closing (prefix default-close other-open other-close) 
   (insert default-close) 
   (unless prefix 
     (let ((open-pt (condition-case nil 
			(scan-sexps (point) -1) 
		      (error (beep) nil)))) 
       (when open-pt 
	 (let ((open-char (aref (buffer-substring-no-properties
				 open-pt (1+ open-pt))
				0))) 
	   (when (= open-char other-open) 
	     (delete-backward-char 1) 
	     (insert other-close)))))))

(defun scheme-insert-closing-paren (&optional prefix) 
  (interactive "P") 
  (scheme-insert-closing prefix ?\) ?\[ ?\]))  

(setq scheme-keybind
      '((")" . scheme-insert-closing-paren) 
    ;;     ("C-c C-l" . (lambda () (interactive) 
    ;;                     (let ((file (buffer-file-name)))
    ;;                       (if file 
    ;;                           (scheme-load-file file)
    ;;                         (scheme-send-region (point-min) (point-max))))))
	;; ("C-c S" . scheme-other-window)
	;; ("C-c C-S" . scheme-other-frame)
	;;("\C-c\C-k" . scheme-kill-repl)
        ))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?@ "w" scheme-mode-syntax-table)
	    (modify-syntax-entry ?| "w" scheme-mode-syntax-table)
	    (define-many-keys scheme-mode-map scheme-keybind)
        ))

(defmacro set-put (progname edit-list)
  "edit-list = ((<value> . (sym ...)) ...)"
  `(progn
     ,@(loop for (depth sym-list) in edit-list nconc
	     (loop for sym in sym-list collect `(put (quote ,sym) ,progname ,depth)))))

(set-put 'scheme-indent-function
	 ((1
	   (guard with-locking-mutex with-signal-handlers with-time-counter with-string-io with-port-locking with-output-to-string with-output-to-process with-output-to-port with-output-conversion with-module with-iterator with-input-from-string with-input-from-process with-input-from-port with-input-conversion with-error-to-port with-builder while when until unless syntax-rules rxmatch-case parse-options parameterize match make letrec-syntax let/cc let-values let-syntax let*-values dotimes dolist call-with-values call-with-temporary-file call-with-output-file call-with-output-conversion call-with-iterator call-with-input-string call-with-input-process call-with-input-file call-with-input-conversion call-with-client-socket and-let* port-fold port-fold-right))
	  (2 
	   (rxmatch-let rxmatch-if receive multiple-value-bind match-let1 let1 let-optionals* let-match let-keywords let-keywords* let-args if-match))
	  (0 
	   (with-error-handler rxmatch-cond call-with-output-string begin0))))
