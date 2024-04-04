;;; -*- coding: utf-8; lexical-binding: t -*-

;; (require 'pickup)
(autoload 'pickup:pickup-file (concat (current-directory) "pickup.el"))

(cl-defun my:browse-github (&key (branch nil) (rel-path nil) (line-no nil))
  (interactive)
  (cl-destructuring-bind (_ . path) (project-current)
    (when-let* ((url (my:resolve-github-url path)))
      (let* ((branch (or branch (car (vc-git-branches))))
             (rel-path (or rel-path (replace-regexp-in-string (expand-file-name path) "" (buffer-file-name))))
             (line-no (or line-no (line-number-at-pos)))
             (full-url (format "%s/blob/%s/%s#L%d" url branch rel-path line-no)))
        (browse-url full-url))))
  )

(defun my:resolve-github-url (&optional path)
  (when-let* ((git-config-file (pickup:pickup-file ".git/config" :path path)))
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
          )))))




