;; rust-mode
;; flycheck-rust
;; company-racer
;; rustfmt

;; flycheck,eldoc,company,tag-jump
(defun my:rust-instal-packages ()
  (interactive)
  (progn
    (package-install 'rust-mode)
    (package-install 'racer)
    (package-install 'flycheck-rust)
    (package-install 'company-racer)
    )
  )

(defun my:rust-setup ()
  (flycheck-mode 1)
  (flycheck-rust-setup)

  (racer-mode 1)
  (eldoc-mode 1)

  (setq my:rust-key-pair
        '(("(" . ")")
          ("\"" . "\"")
          ("'" . "'")
          ("{"  "}" "{")
          ("[" "]" "["))
        )
  (define-insert-pair-binding rust-mode-map my:rust-key-pair)

  (define-many-keys rust-mode-map
    '(
      ("C-x C-s" . rust-format-buffer)
      ("TAB" . company-indent-or-complete-common)
    ))
  (setq company-tooltip-align-annotations t)
  (company-mode-on)
  )

(use-package rust-mode
  :commands (rust-mode)
  :init
  (add-hook 'rust-mode-hook 'my:rust-setup)
  :config
  ;; overwrite rustfmt with `--force` (not rustfmt-nightly)
(defun rust--format-call (buf)
  "Format BUF using rustfmt."
  (with-current-buffer (get-buffer-create "*rustfmt*")
    (erase-buffer)
    (insert-buffer-substring buf)
    (let* ((tmpf (make-temp-file "rustfmt"))
           (ret (call-process-region (point-min) (point-max) rust-rustfmt-bin
                                     t `(t ,tmpf) nil "--force")))
      (unwind-protect
          (cond
           ((zerop ret)
            (if (not (string= (buffer-string)
                              (with-current-buffer buf (buffer-string))))
                (copy-to-buffer buf (point-min) (point-max)))
            (kill-buffer))
           ((= ret 3)
            (if (not (string= (buffer-string)
                              (with-current-buffer buf (buffer-string))))
                (copy-to-buffer buf (point-min) (point-max)))
            (erase-buffer)
            (insert-file-contents tmpf)
            (error "Rustfmt could not format some lines, see *rustfmt* buffer for details"))
           (t
            (erase-buffer)
            (insert-file-contents tmpf)
            (error "Rustfmt failed, see *rustfmt* buffer for details"))))
      (delete-file tmpf))))
  )
