;;; -*- coding: utf-8; lexical-binding: t -*-
(eval-when-compile (require 'cl-lib))

;;----------------------------------------
;; format buffer
;;----------------------------------------

(defun my:masking-home-directory ()
  (interactive)
  (let ((home (getenv "HOME"))
        (repo (base64-decode-string "d2NsNDgvYWktYW5hbHlzdA==")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward repo nil t 1)
        (replace-match "podhmo/app"))
      (goto-char (point-min))
      (while (search-forward home nil t 1)
        (replace-match "$HOME")))))

(defun my:format-buffer ()
  (interactive)
  (my:masking-home-directory)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\\t" nil t 1)
      (replace-match "\t"))
    (goto-char (point-min))
    (while (search-forward "\\n" nil t 1)
      (replace-match "\n"))))


(autoload 'ansi-color-apply-on-region "ansi-color")
(defun my:ansi-color-highlight ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))


;;----------------------------------------
;; github
;;----------------------------------------

(cl-defun my:browse-github (&key (branch nil) (rel-path nil) (line-no nil))
  (interactive)
  (and-let*
      ((root  (vc-git-root default-directory))
       (url (my:resolve-github-url  (format "%s.git/config" root ))))
    (let* ((branch (or branch (car (vc-git-branches))))
           (rel-path (or rel-path (replace-regexp-in-string (expand-file-name  root) "" (buffer-file-name))))
           (line-no (or line-no (line-number-at-pos)))
           (full-url (format "%s/blob/%s/%s#L%d" url branch rel-path line-no)))
      (browse-url full-url))))

(defun my:resolve-github-url (git-config-file)
  (cl-block b
    (let ((buf (find-file-noselect git-config-file)))
      (with-current-buffer buf
        (goto-char (point-min))
        (when (re-search-forward "url = git@github.com:\\(.+\\)\\(?:\\.git\\)?" nil t)
          (let ((user-repository-name (string-trim-right (match-string-no-properties 1) "\\.git$")))
            (cl-return-from b (format "https://github.com/%s" user-repository-name))))
        (goto-char (point-min))
        (when (re-search-forward "url = ssh://git@github.com/\\(.+\\)\\(?:\\.git\\)?" nil t)
          (let ((user-repository-name (string-trim-right (match-string-no-properties 1) "\\.git$")))
            (cl-return-from b (format "https://github.com/%s" user-repository-name))))
        (goto-char (point-min))
        (when (re-search-forward "url = https://github.com/\\(.+\\)\\(?:\\.git\\)?" nil t)
          (let ((user-repository-name (string-trim-right (match-string-no-properties 1) "\\.git$")))
            (cl-return-from b (format "https://github.com/%s" user-repository-name))))
        ))))

;;----------------------------------------
;; temp buffer
;;----------------------------------------

(cl-defun side-pocket:toggle-filename (fname &key (marker "tmp"))
  "toggle file name.  e.g. foo.txt <-> foo.tmp.txt"
  (let* ((parts (split-string (file-name-nondirectory fname) "\\." t))
         (new-name (string-join (cl-remove-if (lambda (x) (string-equal x marker)) parts) ".")))
    (cond ((not (null (member marker parts))) (concat (file-name-directory fname) new-name)) ;; e.g. foo.txt -> foo.tmp.txt
          (t (concat (file-name-directory fname)  (file-name-sans-extension new-name) "." marker "."  (file-name-extension new-name)))))) ;; e.g. foo.tmp.txt -> foo.txt
(defun side-pocket:toggle-buffer () (interactive)
       (when-let ((fname (buffer-file-name)))
         (find-file  (side-pocket:toggle-filename fname :marker "tmp"))))



(provide 'interactives)
