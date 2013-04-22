;; auto-save-buffers
(defvar auto-save-buffers-timer nil)

(defun auto-save-buffer-activate () (interactive)
   (let1 n (string-to-int (completing-read "n:" nil))
     (auto-save-buffers-start n))
   (message "auto-save-buffer is active"))

(defun auto-save-buffer-deactivte () (interactive)
  (cancel-timer auto-save-buffers-timer)
  (setq auto-save-buffers-timer nil)
  (message "auto-save-buffer is deactive"))

(def-toggle auto-save-buffers-toggle
  (:on (auto-save-buffer-activate))
  (:off (auto-save-buffer-deactivte)))

(defun auto-save-buffers-start (delay)
  (setq auto-save-buffers-timer
	(run-with-idle-timer delay t 'auto-save-buffers)))

(defun auto-save-buffers ()
  (loop for buf in (remove-if-not 'buffer-file-name (buffer-list))
	when (and (buffer-modified-p buf)
		  (not buffer-read-only)
		  (not (string-match "^#" (buffer-name buf)))
		  (file-writable-p (buffer-file-name buf)))
	do (with-current-buffer buf 
         (save-buffer))))

(defun auto-save-buffer-deactivte-confirm ()
  (rlet1 status (y-or-n-p "do you deactivate auto-save timer?")
    (when status
      (auto-save-buffer-deactivte))))

(progn ;; patch-for-invalid-coding-system-when-auto-save
  (require 'mule)

  (defun my:select-safe-coding-system (from to &optional default-coding-system
				       accept-default-p file)
  "original is `select-safe-coding-system'"
  (if (and default-coding-system
	   (not (listp default-coding-system)))
      (setq default-coding-system (list default-coding-system)))

  (let ((no-other-defaults nil)
	auto-cs)
    (unless (or (stringp from) find-file-literally)
      ;; Find an auto-coding that is specified for the the current
      ;; buffer and file from the region FROM and TO.
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char from)
	  (setq auto-cs (find-auto-coding (or file buffer-file-name "")
					  (- to from)))
	  (if auto-cs
	      (if (coding-system-p (car auto-cs))
		  (setq auto-cs (car auto-cs))
		(display-warning
		 'mule
		 (format "\
Invalid coding system `%s' is specified
for the current buffer/file by the %s.
It is highly recommended to fix it before writing to a file."
			 (car auto-cs)
			 (if (eq (cdr auto-cs) :coding) ":coding tag"
			   (format "variable `%s'" (cdr auto-cs))))
		 :warning)
		;; (or (yes-or-no-p "Really proceed with writing? ")
		;;     (error "Save aborted"))
		(setq auto-cs nil))))))

    (if (eq (car default-coding-system) t)
	(setq no-other-defaults t
	      default-coding-system (cdr default-coding-system)))

    ;; Change elements of the list to (coding . base-coding).
    (setq default-coding-system
	  (mapcar (function (lambda (x) (cons x (coding-system-base x))))
		  default-coding-system))

    (if (and auto-cs (not no-other-defaults))
	;; If the file has a coding cookie, try to use it before anything
	;; else (i.e. before default-coding-system which will typically come
	;; from file-coding-system-alist).
	(let ((base (coding-system-base auto-cs)))
	  (or (memq base '(nil undecided))
	      (rassq base default-coding-system)
	      (push (cons auto-cs base) default-coding-system))))

    (unless no-other-defaults
      ;; If buffer-file-coding-system is not nil nor undecided, append it
      ;; to the defaults.
      (if buffer-file-coding-system
	  (let ((base (coding-system-base buffer-file-coding-system)))
	    (or (eq base 'undecided)
		(rassq base default-coding-system)
		(setq default-coding-system
		      (append default-coding-system
			      (list (cons buffer-file-coding-system base)))))))

      (unless (and buffer-file-coding-system-explicit
		   (cdr buffer-file-coding-system-explicit))
	;; If default-buffer-file-coding-system is not nil nor undecided,
	;; append it to the defaults.
	(if default-buffer-file-coding-system
	    (let ((base (coding-system-base default-buffer-file-coding-system)))
	      (or (eq base 'undecided)
		  (rassq base default-coding-system)
		  (setq default-coding-system
			(append default-coding-system
				(list (cons default-buffer-file-coding-system
					    base)))))))

	;; If the most preferred coding system has the property mime-charset,
	;; append it to the defaults.
	(let ((preferred (coding-system-priority-list t))
	      base)
	  (and (coding-system-p preferred)
	       (setq base (coding-system-base preferred))
	       (coding-system-get preferred :mime-charset)
	       (not (rassq base default-coding-system))
	       (setq default-coding-system
		     (append default-coding-system
			     (list (cons preferred base))))))))

    (if select-safe-coding-system-accept-default-p
	(setq accept-default-p select-safe-coding-system-accept-default-p))

    ;; Decide the eol-type from the top of the default codings,
    ;; buffer-file-coding-system, or
    ;; default-buffer-file-coding-system.
    (if default-coding-system
	(let ((default-eol-type (coding-system-eol-type
				 (caar default-coding-system))))
	  (if (and (vectorp default-eol-type) buffer-file-coding-system)
	      (setq default-eol-type (coding-system-eol-type
				      buffer-file-coding-system)))
	  (if (and (vectorp default-eol-type) default-buffer-file-coding-system)
	      (setq default-eol-type (coding-system-eol-type
				      default-buffer-file-coding-system)))
	  (if (and default-eol-type (not (vectorp default-eol-type)))
	      (dolist (elt default-coding-system)
		(setcar elt (coding-system-change-eol-conversion
			     (car elt) default-eol-type))))))

    (let ((codings (find-coding-systems-region from to))
	  (coding-system nil)
	  (tick (if (not (stringp from)) (buffer-chars-modified-tick)))
	  safe rejected unsafe)
      (if (eq (car codings) 'undecided)
	  ;; Any coding system is ok.
	  (setq coding-system (caar default-coding-system))
	;; Reverse the list so that elements are accumulated in safe,
	;; rejected, and unsafe in the correct order.
	(setq default-coding-system (nreverse default-coding-system))

	;; Classify the defaults into safe, rejected, and unsafe.
	(dolist (elt default-coding-system)
	  (if (or (eq (car codings) 'undecided)
		  (memq (cdr elt) codings))
	      (if (and (functionp accept-default-p)
		       (not (funcall accept-default-p (cdr elt))))
		  (push (car elt) rejected)
		(push (car elt) safe))
	    (push (car elt) unsafe)))
	(if safe
	    (setq coding-system (car safe))))

      ;; If all the defaults failed, ask a user.
      (when (not coding-system)
	(setq coding-system (select-safe-coding-system-interactively
			     from to codings unsafe rejected (car codings))))

      ;; Check we're not inconsistent with what `coding:' spec &c would
      ;; give when file is re-read.
      ;; But don't do this if we explicitly ignored the cookie
      ;; by using `find-file-literally'.
      (when (and auto-cs
		 (not (and
		       coding-system
		       (memq (coding-system-type coding-system) '(0 5)))))
	;; Merge coding-system and auto-cs as far as possible.
	(if (not coding-system)
	    (setq coding-system auto-cs)
	  (if (not auto-cs)
	      (setq auto-cs coding-system)
	    (let ((eol-type-1 (coding-system-eol-type coding-system))
		  (eol-type-2 (coding-system-eol-type auto-cs)))
	    (if (eq (coding-system-base coding-system) 'undecided)
		(setq coding-system (coding-system-change-text-conversion
				     coding-system auto-cs))
	      (if (eq (coding-system-base auto-cs) 'undecided)
		  (setq auto-cs (coding-system-change-text-conversion
				 auto-cs coding-system))))
	    (if (vectorp eol-type-1)
		(or (vectorp eol-type-2)
		    (setq coding-system (coding-system-change-eol-conversion
					 coding-system eol-type-2)))
	      (if (vectorp eol-type-2)
		  (setq auto-cs (coding-system-change-eol-conversion
				 auto-cs eol-type-1)))))))

	(if (and auto-cs
		 ;; Don't barf if writing a compressed file, say.
		 ;; This check perhaps isn't ideal, but is probably
		 ;; the best thing to do.
		 (not (auto-coding-alist-lookup (or file buffer-file-name "")))
		 (not (coding-system-equal coding-system auto-cs)))
	    (unless (yes-or-no-p
		     (format "Selected encoding %s disagrees with \
%s specified by file contents.  Really save (else edit coding cookies \
and try again)? " coding-system auto-cs))
	      (error "Save aborted"))))
      (when (and tick (/= tick (buffer-chars-modified-tick)))
	(error "Cancelled because the buffer was modified"))
      coding-system)))

  (setq select-safe-coding-system-function 'my:select-safe-coding-system))
