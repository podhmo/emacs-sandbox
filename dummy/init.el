(princ "readed")
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
      default-directory))

(add-to-list 'load-path (current-directory))

(load "hello")
