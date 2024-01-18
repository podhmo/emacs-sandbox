(defvar ctl-j-map (make-keymap))

(defvar on-after-keyboard-setup (list))
(defvar on-before-keyboard-setup (list))
(defmacro with-before-keyboard-setup (&rest body)
  `(add-hook 
    'on-before-keyboard-setup
    (lambda ()
      ,@body)))
(defmacro with-after-keyboard-setup (&rest body)
  `(add-hook 
    'on-after-keyboard-setup
    (lambda ()
      ,@body)))

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-mode 1)

  (key-chord-define-global "jk" 'view-mode)
  ;; (key-chord-define-global "po" 'org-remember)
  (key-chord-define-global "po" 'monologue)
)

(use-package shell
  :init
  (defun my:shell-mode-setup ()
    (bind-keys :map shell-mode-map
               ;; ("C-p" . comint-previous-input)
               ;; ("C-n" . comint-next-input)
               ([up] . comint-previous-input)
               ([down] . comint-next-input)))
  (add-hook 'shell-mode-hook 'my:shell-mode-setup)
  )


;;C-gを押したときに現在の入力がヒストリーに記録されるようになる。間違ってC-gを押してしまった場合は、再び同じコマンドを起動してM-pで前の入力を呼び戻せる
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))


;; スクリプトを保存するとき()ファイルの先頭に #! が含まれているとき)，自動的に chmod +x を行う
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package zlc
  :ensure t
  :config
  (bind-keys :map minibuffer-local-map
             ;; ("C-f" . zlc-select-next)
             ;; ("C-b" . zlc-select-previous)
             ("C-p" . zlc-select-previous-vertical)
             ("C-n" . zlc-select-next-vertical))
  (zlc-mode t))

(use-package eldoc
  :config
  (setq eldoc-argument-case 'downcase)

  (defadvice  eldoc-get-fnsym-args-string
      (around eldoc-named-progn-display-section (sym &optional index) activate)
    (cond ((eq sym 'named-progn)
           (let ((section-name
                  (save-excursion
                    (eldoc-beginning-of-sexp)
                    (goto-char (scan-sexps (point) 1))
                    (skip-syntax-forward "^w_")
                    (thing-at-point 'symbol))))
             (message "progn ;; -- %s --" section-name)))
          (t ad-do-it))))

(progn ;; anything
  (require-and-fetch-if-not 'anything :url "https://raw.githubusercontent.com/emacsattic/anything/master/anything.el")
  (require-and-fetch-if-not 'anything-match-plugin :url "https://raw.githubusercontent.com/emacsattic/anything/master/anything-match-plugin.el")
  (defvar browse-url-mosaic-program "mosaic")
  (require-and-fetch-if-not 'anything-config :url "https://raw.githubusercontent.com/emacsattic/anything/master/anything-config.el")

  ;; (require 'anything-complete)

  (setq anything-execute-action-at-once-if-one t)

  (require 'outline)
  ;; (defadvice anything-next-line (after execute-persistent-action disable)
  ;;   (unless (or (anything-get-previous-header-pos)
  ;;               (anything-get-next-header-pos))
  ;;     (call-interactively 'anything-execute-persistent-action)))

  ;; (defadvice anything-previous-line (after execute-persistent-action disable)
  ;;   (unless (or (anything-get-previous-header-pos)
  ;;               (anything-get-next-header-pos))
  ;;     (call-interactively 'anything-execute-persistent-action)))

  ;; (defmacro with-anything-line-move-advice (advice-name action)
  ;;   `(progn
  ;;      (ad-enable-advice 'anything-next-line 'after ',advice-name)
  ;;      (ad-activate 'anything-next-line)
  ;;      (ad-enable-advice 'anything-previous-line 'after ',advice-name)
  ;;      (ad-activate 'anything-previous-line)
  ;;      (cl-letf (((symbol-function 'message) (lambda (&rest args)))
  ;;                (unwind-protect
  ;;                    ,action
  ;;                  (progn (ad-deactivate 'anything-previous-line)
  ;;                         (ad-deactivate 'anything-next-line)))))))
  (defmacro with-anything-line-move-advice (advice-name action)
    (progn ,advice-name ,action))
  (defun anything-occur* ()
    "Preconfigured Anything for Occur source."
    (interactive)
    (with-anything-line-move-advice
     execute-persistent-action
     (anything-occur)))

  (progn ;; bookmarking-current-buffer
    (use-package bm
      :ensure t)

    (defun anything-bm-list* ()
      "Preconfigured `anything' for visible bookmarks."
      (interactive)
      (with-anything-line-move-advice
       execute-persistent-action
       (anything-bm-list)))
    )
  (progn ;; key-settings/anything
    (add-hook 'on-after-keyboard-setup
              (lambda ()
                (define-many-keys global-map
                  '(("<hiragana-katakana>" . newline)
                    ("<henkan>" . toggle-input-method)
                    ("<muhenkan>" . delete-backward-char)
                    ("C-c C-a" . anything)
                    ("C-c C-;" . anything-vcs-project)
                    ("C-c C-:" . anything-vcs-project)
                    ;; ("M-x" . anything-M-x) ;; use counsel-M-x
                    ("C-x b" . anything-buffers+)
                    ;; ("M-y" . anything-show-kill-ring) use counsel-yank-pop
                    ("C-j C-j" . anything-bm-list*)
                    ("C-j j" . bm-toggle)
                    ))))
    )
  )


(progn ;; viewer-mode-settings
  (progn ;; for-buffer-file-permission
    (setq view-read-only t)
    (defadvice find-file
        (around find-file-switch-to-view-file (file &optional wild) activate)
      (if (and (not (file-writable-p file))
               (not (file-directory-p file)))
          (view-file file)
        ad-do-it))

    (defvar view-mode-force-exit nil)
    (defmacro do-not-exit-view-mode-unless-writable-advice (f)
      `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
         (if (and (buffer-file-name)
                  (not view-mode-force-exit)
                  (not (file-writable-p (buffer-file-name))))
             (message "File is unwritable, so stay in view-mode.")
           ad-do-it)))
    (do-not-exit-view-mode-unless-writable-advice view-mode-exit)
    (do-not-exit-view-mode-unless-writable-advice view-mode-disable))

  (progn ;; keyboard-settings/viewer-mode
    (defmacro funcall&viewer-mode (func) ;;slack-off
      `(lambda () (interactive) ,func (view-mode-enable)))

    (setq pager-keysettings
          `( ;; vi-like
            ("/" . re-search-forward)
            ("h" . backward-word)
            ("l" . forward-word)
            ("j" . next-line)
            ("k" . previous-line)
            ;; ("j" . inertias-up)
            ;; ("k" . inertias-down)
            ("^" . move-beginning-of-line)
            ("$" . move-end-of-line)
            ("(" . point-undo)
            (")" . point-redo)
            ("i" . ,(funcall&viewer-mode (other-window -1)))
            ("o" . ,(funcall&viewer-mode (other-window 1)))
            ;; convinience
            (";" . ,(funcall&viewer-mode (elscreen-previous)))
            (":" . ,(funcall&viewer-mode (elscreen-next)))
            (" " . View-scroll-half-page-forward)
            ("b" . View-scroll-half-page-backward)
            ("C-j" . nil)
            ;; vim like
            ("g" . beginning-of-buffer)
            ("G" . end-of-buffer)
            ;; langhelp-like
            ("c" . scroll-other-window-down)
            ("v" . scroll-other-window)
            ;; manipurate other frame
            ("a" . help-go-back-from-anywhere)
            ("s" . help-go-forward-from-anywhere)
            ))
    (add-hook 'view-mode-hook
              (lambda ()
                (define-many-keys view-mode-map pager-keysettings)))

    ))

(progn ;; elscreen
  (progn ;; daily-commands
    (defun elscreen-shell/next-screen () (interactive)
           "create shell buffer with current directory as pwd"
           (let1 dir (current-directory)
             (elscreen-create)
             (shell)
             (comint-simple-send (get-buffer-process dir)
                                 (concat "cd " dir))
             (goto-char (point-max)))))

  (use-package elscreen
    :ensure t
    :init
    (defun global-j-define-key (&optional kmap)
      (and-let* ((kmap (or kmap (current-local-map))))
        (define-key kmap "\C-j" nil))) ;; experimental

    (add-hook 'on-before-keyboard-setup
              (lambda ()
                (defadvice elscreen-goto (after kill-Cj  activate)
                  (global-j-define-key))
                (defadvice switch-to-buffer (after kill-Cj  activate)
                  (global-j-define-key))
                (setq elscreen-prefix-key (kbd "C-j"))
                (elscreen-start)))
    )
  ;;
  (when (equal system-type 'darwin)
    (defun my:override-elscreen-setup (key-map)
      (define-key key-map (kbd "C-'") 'elscreen-next)
      )
    (my:override-elscreen-setup global-map)
    (with-eval-after-load 'org
      (add-hook 'org-mode-hook (lambda () (my:override-elscreen-setup org-mode-map)))
      )
    )
  )

;; supress bell sound
(defun silent-bell () (message "bell!"))
(setq ring-bell-function 'silent-bell)

;; monologue
(use-package monologue ;; mine
  :commands (monologue)
  :bind (("C-c C-w" . monologue))
)

(progn ;; dired
  (defun my:dired-setup ()
    (define-many-keys dired-mode-map
      `(
        ("[" . dired-up-directory) ;; ^
        ("]" . my:dired-do-redisplay-or-down-directory)
        ("h" . dired-up-directory) ;; ^
        ("l" . my:dired-do-redisplay-or-down-directory)
        ("j" . dired-next-line)
        ("k" . dired-previous-line)
        ("J" . dired-next-dirline) ;; >
        ("K" . dired-prev-dirline) ;; <
        ))
    )

  (defun my:dired-do-redisplay-or-down-directory ()
    (interactive)
    (let ((fpath (dired-get-file-for-visit)))
      (cond ((file-directory-p fpath) (call-interactively 'dired-find-file))
            (t (call-interactively 'dired-do-redisplay)))
      )
    )
  (add-hook 'dired-mode-hook 'my:dired-setup)
  )

(progn ;; help
  (defun my:help-setup ()
    (define-many-keys help-mode-map
      `(
        ("[" . help-go-back)
        ("]" . help-go-forward)
        ))
    )
  (add-hook 'help-mode-hook 'my:help-setup)
  )


(progn ;; font-size

  (setq my:font-height-candidates '(140 220))

  (lexical-let ((i 0))
    (defun* my:font-height (&optional incp &key candidates)
      (when incp
        (setq i (+ i (if (numberp incp) incp 1))))

      (let* ((candidates (or candidates my:font-height-candidates))
             (height (nth i candidates)))
        (cond ((numberp height) height)
              (t
               (setq i 0)
               (nth i candidates))))))

  (defun my:adjust-font-height (arg)
    (interactive "p")
    (let* ((incp (+ 1 (/ arg 4)))
           (font-height (my:font-height incp)))
      (message "remap my font height %d (inc %s)" font-height incp)
      (face-remap-add-relative 'default :height font-height)
      ))

  (defun my:adjust-font-height-globally ()
    (interactive)
    (let* ((incp nil)
           (font-height (my:font-height incp)))
      (message "remap my font height %d globally (inc %s)" font-height incp)

      ;; clear all temporary remapping setting
      (dolist (b (buffer-list))
        (with-current-buffer b
          (when face-remapping-alist
            (setq-local face-remapping-alist nil)
            )
          ))

      (set-face-attribute 'default nil :height font-height)
      ))
  )


(use-package undo-tree
  :ensure t
  ;; https://emacs.stackexchange.com/questions/47940/undo-tree-mode-only-goes-back-a-small-number-of-steps-when-using-ensime-then
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              ;; 10X bump of the undo limits to avoid issues with premature
              ;; Emacs GC which truncages the undo history very aggresively
              undo-limit 800000
              undo-strong-limit 12000000
              undo-outer-limit 120000000)
  :config
  ;; C-x u undo-tree-visualize
  ;; C-. redo
  ;; C-/ undo
  (global-undo-tree-mode))


;;; high cost
;; (use-package highlight-indent-guides
;;   :ensure t
;;   :diminish
;;   :hook
;;   ((prog-mode yaml-mode) . highlight-indent-guides-mode)
;;   :custom
;;   (highlight-indent-guides-auto-enabled t)
;;   (highlight-indent-guides-responsive t)
;;   (highlight-indent-guides-method 'character) ; column
;;   )


(use-package volatile-highlights
  :ensure t
  :diminish
  :hook
  (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
  :config

  (with-eval-after-load 'undo-tree
    (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move 'undo-tree-redo  'undo-tree-undo)
    (vhl/install-extension 'undo-tree)
    )
  (volatile-highlights-mode t)
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  )

;; ivy families
;; new completion interface
(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  ;; please set enable-recursive-minibuffers to t
  (setq ivy-extra-directories nil)
  (setq ivy-height 7)
  (setq ivy-format-function #'ivy-format-function-arrow)

  (defun my:ivy-mode-setup ()
    ;; for find-file-at-point
    (setq completing-read-function 'completing-read-default)
    (ivy-rich-mode 1)
    )
  (add-hook 'ivy-mode-hook 'my:ivy-mode-setup)

  (use-package ivy-rich
    :ensure t
    :commands (ivy-rich-mode)
    )

  ;; Notice: not in ivy-mode, the setting for display-function is not activated, yet
  ;; (when calling ivy-posframe-display-at-point, and the help message is displayed for current cursor symbol, then move to this)
  (use-package ivy-posframe
    :ensure t
    :commands (ivy-postframe-enable)
    :after ivy
    :config
    (setq ivy-display-function nil) ; default

    (add-to-list 'ivy-display-functions-alist '(complete-symbol . ivy-posframe-display-at-point))
    )
  )

(use-package swiper
  :ensure t
  :bind (("M-s" . swiper))
  )

(use-package counsel
  :ensure t
  :bind (;("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-x m" . counsel-imenu)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         )
  :config
  ;; (setq ffap-file-finder 'counsel-find-file)
  :custom
  (counsel-yank-pop-separator "\n----------------------------------------\n")
  )

