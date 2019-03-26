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


(cond
 ((or (require 'eglot nil t) nil)
  (load (format "%smygo/modern.el" (current-directory))))
 (t
  (load (format "%smygo/legacy.el" (current-directory)))
  ))
