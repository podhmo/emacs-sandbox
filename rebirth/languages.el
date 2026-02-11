;;; -*- coding: utf-8; lexical-binding: t -*-

;;-----------------------------------------
;; elisp
;;-----------------------------------------

(defun my:elisp-pretty-print-region (beg end)
  (interactive
   (list
    (if (use-region-p) (region-beginning) (point-min))
    (if (use-region-p) (region-end) (point-max))))
  (save-excursion
    (unwind-protect
	(progn
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (while (re-search-forward "[  	]+$" nil t 1)
	    (replace-match ""))
	  (indent-region (point-min) (point-max)))
      (widen))))


;;-----------------------------------------
;; make
;;-----------------------------------------

(with-eval-after-load 'make-mode
  ;; auto-save中で定期的にy-or-n-pで尋ねてくるのはうるさすぎるので *Messages* に書くだけにする
  (defun makefile-warn-suspicious-lines ()
    ;; Returning non-nil cancels the save operation
    (if (derived-mode-p 'makefile-mode)
        (save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward "^\\(\t+$\\| +\t\\)" nil t)
	      (message "Suspicious line %d. Save anyway? "
		       (count-lines (point-min) (point)))))))
  (defun makefile-warn-continuations ()
    (if (derived-mode-p 'makefile-mode)
        (save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward "\\\\[ \t]+$" nil t)
	      (message "Suspicious continuation in line %d. Save anyway? "
		       (count-lines (point-min) (point)))))))
  )
