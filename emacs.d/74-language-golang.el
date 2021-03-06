(defvar my:eglot-disable-mapping (make-hash-table :test 'equal))

(defun my:eglot-ensure ()
  (interactive)
  (let ((disabled (gethash major-mode my:eglot-disable-mapping)))
    (unless disabled
      (eglot-ensure))
    (message "eglot disabled? %s" disabled)))

(defun my:eglot-disable ()
  (interactive)
  (puthash major-mode t my:eglot-disable-mapping)

  (when (boundp 'eglot--servers-by-project)
    (cl-loop
     for servers being the hash-values of eglot--servers-by-project
     do (cl-loop
         for server in servers
         when (equal (eglot--major-mode server) major-mode)
         do (eglot-shutdown server))))
  (revert-buffer nil t))

(defun my:eglot-enable ()
  (interactive)
  (puthash major-mode nil my:eglot-disable-mapping)
  (revert-buffer nil t)
  )

(defun my:go-path ()
  (let ((output (shell-command-to-string "$SHELL --login -i -c 'echo $GOPATH'")))
    (or (car (last (split-string output)))
        (progn
          (unless (member (expand-file-name "~/go/bin") exec-path)
            (push (expand-file-name "~/go/bin") exec-path)
            )
          (expand-file-name "~/go")))))

(defvar my:go-format-buffer-function 'gofmt)
(defun my:go-toggle-format-buffer-key-bind ()
  (interactive)
  (let ((cmd (intern
              (completing-read "bind-key(C-x C-s): " (list "eglot-format-buffer" "gofmt")))))
    (define-key go-mode-map (kbd "C-x C-s") cmd)
    (my:go-setup-format-buffer)))

(defun my:go-setup-format-buffer ()
  (define-key go-mode-map (kbd "C-x C-s") my:go-format-buffer-function))

(defun my:go-setup-gofmt-command ()
  ;; gopath
  (let ((output (shell-command-to-string "$SHELL --login -i -c 'echo $GOPATH'")))
    (setenv "GOPATH" (car (last (split-string output)))))

  (cond ((executable-find "goimports")
         (setq gofmt-command "goimports"))
        (t (message "gorimports > gofmt!!")))
  )


(cond
 ((or (require 'eglot nil t) nil)
  (load (format "%smygo/modern.el" (current-directory))))
 (t
  (load (format "%smygo/legacy.el" (current-directory)))
  ))
