(require 'ffap)

;; dict-support
(setq ffap-url-regexp "\\(dict\\|file\\|ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://")


(global-set-key (kbd "C-j") ctl-j-map)
(defvar keyboard-settings:curdir (current-directory))
(defun keyboard-settings-setup ()
  ;; occur before settings hook
  (run-hook-with-args-until-failure
   'on-before-keyboard-setup)

  (progn ;; global-key-settings ;; or human-interfaces.el
    (setq global-individual-key-mapping
          '(("C-c C-l" . eval-buffer)
            ("M-r" . replace-string)
            ("M-R" . replace-regexp)

            ("C-x C-l" . goto-line)
            ("C-x C-a" . revert-buffer)

            ("C-c C-c" . toggle-file)
            ("C-c C-f" . ffap)
            ("C-c C-e" . eval-defun) ;;
            ("C-c j" . dabbrev-expand)
            ("C-c C-j" . dabbrev-expand) ;;
            ("C-c q" . comment-region)
            ("C-c Q" . uncomment-region)

            ("C-c x" . (lambda () (interactive) (find-file (concat keyboard-settings:curdir "/init.el"))))
            ("C-c e" . enclose-element-interactive)
            ("C-c d" . delete-syntax-forward*)
            ("M-r" . replace-string)
            ("M-R" . replace-regexp)

            ;; quick-run
            ("C-c @" . quickrun-compile-only)
            ("C-c C-@" . quickrun)

            ("C-c C-w" . monologue)

                                        ;elscreen
            ("C-;" . elscreen-previous)
            ("C-:" . elscreen-next)
            ("C-j S" . elscreen-shell/next-screen)
            ("C-j C-f" . elscreen-find-file)              

            ("C-." . redo)
            ("C-/" . undo)
            ("C-j S" . open-shell-with-pwd)
            ("<f5>" . revert-buffer)
            ("<f12>" . (lambda () (interactive)
                         (message "reflesh")
                         (setq extended-command-history nil)))
            )
          )
    (define-many-keys (current-global-map) global-individual-key-mapping))
  (progn ;; key-chord
    (key-chord-define-global "jk" 'view-mode)
;    (key-chord-define-global "po" 'org-remember)
    (key-chord-define-global "po" 'monologue)
    )
  )
