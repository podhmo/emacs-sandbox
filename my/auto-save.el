;; auto-save-buffers
(defvar auto-save-buffers-timer nil)

(def-toggle auto-save-buffers-toggle
  (:on 
   (let1 n (string-to-int (completing-read "n:" nil))
     (auto-save-buffers-start n))
   (message "auto-save-buffer is active"))
  (:off 
   (cancel-timer auto-save-buffers-timer)
    (setq auto-save-buffers-timer nil)
    (message "auto-save-buffer is deactive")))

(defun auto-save-buffers-start (delay)
  (setq auto-save-buffers-timer
	(run-with-idle-timer delay t 'auto-save-buffers)))

(defun auto-save-buffers ()
  (loop for buf in (remove-if-not 'buffer-file-name (buffer-list))
	when (and (buffer-modified-p buf)
		  (not buffer-read-only)
		  (not (string-match "^#" (buffer-name buf)))
		  (file-writable-p (buffer-file-name buf)))
	do (with-current-buffer buf(save-buffer))))
