;;; debug utiltities
(defvar cmh:debug-buffer nil)
(defvar cmh:me nil)
(defvar cmh:debug-buffer-name "*cmh:debug*")
(defvar cmh:is-display-when-debug-log t)
(defmacro cmh:with-debug-buffer (&rest body)
  `(progn
     (unless cmh:debug-buffer
       (setq cmh:debug-buffer (get-buffer-create cmh:debug-buffer-name)))
     (with-current-buffer cmh:debug-buffer
       ,@body)))

(defun cmh:debug-message-default ()
  (let1 time (format-time-string "/%Y/%m/%d %H:%M:%S" (current-time))
    (format "%s mode: (cmh:%s, %s) buffer:(%s)\n"
            time cmh:current-major-mode major-mode
            (current-buffer))))

(defvar cmh:debug-message-function 'cmh:debug-message-default)

(defun* cmh:debug-current-state (&optional (prefix "") (suffix ""))
  (let1 message (funcall cmh:debug-message-function)
    (cmh:with-debug-buffer
     (goto-char (point-max))
     (insert prefix message suffix)))
  (when cmh:is-display-when-debug-log
    (display-buffer cmh:debug-buffer)))

(defun cmh:debug-current-state* ()
  (cmh:debug-current-state (concat cmh:me "\t")))

(defun change-mode-hook-debug-activate () (interactive)
  (add-hook 'cmh:change-mode-hook 'cmh:debug-current-state*))

(defun change-mode-hook-debug-deactivate () (interactive)
  (remove-hook 'cmh:change-mode-hook 'cmh:debug-current-state*))

;;; code:
(defvar cmh:change-mode-hook nil)
(defvar cmh:current-major-mode nil)
;; (defvar cmh:exclude-major-mode-list '(lisp-interaction-mode))

(defadvice other-window (around cmh:hook-other-window activate)
  (setq cmh:me "other-window")
  ad-do-it
  (unless (eq cmh:current-major-mode major-mode)
    (run-hooks 'cmh:change-mode-hook)
    (setq cmh:current-major-mode major-mode)))

;;; switch-to-buffer and set-window-configuration are too noizy.
;; (defadvice switch-to-buffer (around cmh:hook-switch-to-buffer activate)
;;   (setq cmh:me "switch-to-buffer")
;;   (setq cmh:current-major-mode major-mode)
;;   ad-do-it
;;   (unless (eq cmh:current-major-mode major-mode)
;;     (run-hooks 'cmh:change-mode-hook)
;;     (setq cmh:current-major-mode major-mode)))
;; (defadvice set-window-configuration (around cmh:hook activate)
;;   (when (and (equal chm:current-buffer (current-buffer))
;;              (not (eq cmh:current-major-mode major-mode)))
;;     (run-hooks 'cmh:before-change-mode-hook))
;;   ad-do-it
;;   (when (and (equal chm:current-buffer (current-buffer))
;;              (not (eq cmh:current-major-mode major-mode)))
;;     (run-hooks 'cmh:before-change-mode-hook)
;;     (setq cmh:current-major-mode major-mode)))

(defadvice find-file (around cmh:hook-find-file activate)
  (setq cmh:me "find-file")
  (setq cmh:current-major-mode major-mode)
  ad-do-it
  (unless (eq cmh:current-major-mode major-mode)
    (run-hooks 'cmh:change-mode-hook)
    (setq cmh:current-major-mode major-mode)))

(when (fboundp 'elscreen-goto)
  (defadvice elscreen-goto (around cmh:hook-elscreen-goto activate)
    (setq cmh:me "elscreen-goto")
    (setq cmh:current-major-mode major-mode)
    ad-do-it
    (unless (eq cmh:current-major-mode major-mode)
      (run-hooks 'cmh:change-mode-hook)
      (setq cmh:current-major-mode major-mode))))

;; (change-mode-hook-debug-activate)
(provide 'change-mode-hook)