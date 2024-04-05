;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'cl-lib)

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
