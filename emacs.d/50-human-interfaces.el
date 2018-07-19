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


(progn ;; key-chord
  (require 'key-chord)
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-mode 1)
  )

(progn ;; multiple-cursors
  (require 'multiple-cursors)
  (progn ;; key-settings/multiple-cursors
    (add-hook 'on-after-keyboard-setup
              (lambda ()
                (define-many-keys global-map
                  '(
                    ("C-x r t" . mc/mark-all-dwim)
                    )
                  )))
    )
  )


;; from: https://github.com/wakaran/config/blob/master/dot.emacs.d.server/init/90-last-setting.el
(progn ;; shell-settings
;;;; shell-modeで上下でヒストリ補完
  ;; C-p/C-nでヒストリを辿る (デフォルトでもM-p, M-nで出来る)
  (add-hook 'on-after-keyboard-setup
            (lambda ()
              (add-hook
               'shell-mode-hook
               (function (lambda ()
                           (define-key shell-mode-map [up] 'comint-previous-input)
                           (define-key shell-mode-map [down] 'comint-next-input)
                           (define-key shell-mode-map "\C-p" 'comint-previous-input)
                           (define-key shell-mode-map "\C-n" 'comint-next-input)))))))

;; ;; kill-ring に同じ内容の文字列を複数入れない
;; ;; kill-ring-save 等した時にその内容が既に kill-ring にある場合、その文字列が kill-ring の先頭に 1 つにまとめられます
;; (defadvice kill-new (before ys:no-kill-new-duplicates disable)
;;   (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;;C-gを押したときに現在の入力がヒストリーに記録されるようになる。間違ってC-gを押してしまった場合は、再び同じコマンドを起動してM-pで前の入力を呼び戻せる
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))


;; スクリプトを保存するとき()ファイルの先頭に #! が含まれているとき)，自動的に chmod +x を行う
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; 新規作成したファイルを未編集でも保存できるようにする
;; http://stackoverflow.com/questions/2592095/how-do-i-create-an-empty-file-in-emacs
;; (add-hook 'find-file-hooks 'assume-new-is-modified)
;; (defun assume-new-is-modified ()
;;   (when (not (file-exists-p (buffer-file-name)))
;;     (set-buffer-modified-p t)))

;; 連続する文末の空行を削除
;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (save-excursion
;;               (goto-char (point-max))
;;               (delete-blank-lines))))

(progn ;; popwin
  (require 'popwin)
  (setq display-buffer-function 'popwin:display-buffer)

  ;; quickrunここがただしい？
  (setq popwin:special-display-config
        '(("*anything*" :height 20)
          ("*quickrun*" :noselect t)
          help-mode
          ;; (completion-list-mode :noselect t)
          ;; (compilation-mode :noselect t)
          (grep-mode :noselect t)
          (occur-mode :noselect t)
          ("*Pp Macroexpand Output*" :noselect t)
          "*Shell Command Output*" "*vc-diff*" "*vc-change-log*"
          (" *undo-tree*" :width 60 :position right)
          ("^\\*anything.*\\*$" :regexp t)
          "*slime-apropos*" "*slime-macroexpansion*" "*slime-description*"
          ("*slime-compilation*" :noselect t)
          "*slime-xref*"
          (sldb-mode :stick t)
          ("*quickrun*" :noselect t)
          slime-repl-mode slime-connection-list-mode))
  
  (progn ;; patch ;; for bag fix
    (when (>= 1 (length (patch:function-arguments 'called-interactively-p)))
      (defun popwin:called-interactively-p ()
        (called-interactively-p))))

  ;; suppress: memory and cpu leak (ad-hoc)
  ;; todo   (or popwin:close-popup-window-timer
  ;; (defadvice popwin:start-close-popup-window-timer (before suppress-timer-leak activate)
  ;;   (when (and (not popwin:popup-window) (not (popwin:popup-window-live-p)))
  ;;     (when (< 0 (random 2))
  ;;       (dolist (tm timer-list)
  ;;         (when (and (eq 'popwin:close-popup-window-timer (timer--function tm))
  ;;                    (not (eq popwin:close-popup-window-timer tm)))
  ;;           (cancel-timer tm)))
  ;;         ) 
  ;;     )
  ;;   )
  (setq popwin:close-popup-window-timer-interval 0.5)
  )

;;; my own
(progn ;; redo
  (require-and-fetch-if-not 'redo+ :url "https://github.com/emacsmirror/emacswiki.org/raw/master/redo%2b.el")
  )
(progn ;; zlc
  (require 'zlc)
  )

;; (progn ;; editing
;;   (require 'autopair)
;;   (setq-default autopair-dont-pair `(:string (?') :comment  (?') :never (?`)))
;;   ;; (require-and-fetch-if-not 'paredit)
;;   )

(progn ;; eye-candy
  (progn ;; eldoc
    (require 'eldoc)
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
            (t ad-do-it)))
    )
  )


(progn ;; anything
  (require-and-fetch-if-not 'anything :url "https://github.com/emacsmirror/emacswiki.org/raw/master/anything.el")
  (require-and-fetch-if-not 'anything-match-plugin :url "https://github.com/emacsmirror/emacswiki.org/raw/master/anything-match-plugin.el")
  (require-and-fetch-if-not 'anything-config :url "https://github.com/emacsmirror/emacswiki.org/raw/master/anything-config.el")

  ;; (require 'anything-complete)

  (setq anything-execute-action-at-once-if-one t)

  (require 'outline)
  (defadvice anything-next-line (after execute-persistent-action disable)
    (unless (or (anything-get-previous-header-pos)
                (anything-get-next-header-pos))
      (call-interactively 'anything-execute-persistent-action)))

  (defadvice anything-previous-line (after execute-persistent-action disable)
    (unless (or (anything-get-previous-header-pos)
                (anything-get-next-header-pos))
      (call-interactively 'anything-execute-persistent-action)))

  (defmacro with-anything-line-move-advice (advice-name action)
    `(progn
       (ad-enable-advice 'anything-next-line 'after ',advice-name)
       (ad-activate 'anything-next-line)
       (ad-enable-advice 'anything-previous-line 'after ',advice-name)
       (ad-activate 'anything-previous-line)
       (flet ((message (&rest args)))
         (unwind-protect
             ,action
           (progn (ad-deactivate 'anything-previous-line)
                  (ad-deactivate 'anything-next-line))))))

  (defun anything-occur* () 
    "Preconfigured Anything for Occur source."
    (interactive)
    (with-anything-line-move-advice 
     execute-persistent-action
     (anything-occur)))

  (progn ;; bookmarking-current-buffer
    (require 'bm)

    (defun anything-bm-list* ()
      "Preconfigured `anything' for visible bookmarks."
      (interactive)
      (with-anything-line-move-advice 
       execute-persistent-action
       (anything-bm-list)))
    )
  (progn ;; popwin
    (when (boundp 'popwin:special-display-config)
      (setq anything-samewindow nil)
      (add-to-list 'popwin:special-display-config '("*anything*" :height 20))))

  (progn ;; key-settings/anything
    (add-hook 'on-after-keyboard-setup
              (lambda ()
                (define-many-keys global-map
                  '(("<hiragana-katakana>" . newline)
                    ("<henkan>" . toggle-input-method)
                    ("<muhenkan>" . delete-backward-char)
                    ("C-c C-a" . anything)
                    ("C-c C-;" . anything-occur*)
                    ("C-c C-:" . anything-vcs-project)
                    ("M-x" . anything-M-x)
                    ("C-x b" . anything-buffers+)
                    ("M-y" . anything-show-kill-ring)
                    ("C-j C-j" . anything-bm-list*)
                    ("C-j j" . bm-toggle)
                    ))))
    )
  )  

;; (progn ;; scroll-buffer 
;;   (require-and-fetch-if-not 'deferred)
;;   (require 'inertial-scroll)
;;   (setq inertias-initial-velocity 50)
;;   (setq inertias-friction 120)
;;   (setq inertias-update-time 50)
;;   (setq inertias-rest-coef 0)
;;   (setq inertias-global-minor-mode-map 
;;         (inertias-define-keymap
;;        '(
;;          ;; Mouse wheel scrolling
;;          ("<wheel-up>"   . inertias-down-wheel)
;;          ("<wheel-down>" . inertias-up-wheel)
;;          ("<mouse-4>"    . inertias-down-wheel)
;;          ("<mouse-5>"    . inertias-up-wheel)
;;          ;; Scroll keys
;;          ("<next>"  . inertias-up)
;;          ("<prior>" . inertias-down)
;;          ("C-v"     . inertias-up)
;;          ("M-v"     . inertias-down)
;;          ) inertias-prefix-key))
;;   (setq inertias-rebound-flash nil)
;;   (inertias-global-minor-mode 1)
;;   )

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

  (progn ;; elscreen
    (require 'elscreen)

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
(defvar monologue:current-buffer-name-function 'current-memo)
(defvar monologue:monologue-partation-string
  "-monologue----------------------------------------\n")

(defun monologue:get-current-buffer ()
  (let ((bufname (funcall monologue:current-buffer-name-function)))
    (find-file-noselect bufname)))

(defun monologue:start-point ()
  (goto-char (point-max))
  (let ((paration-string monologue:monologue-partation-string))
    (cond ((re-search-backward paration-string nil t 1) (forward-line 1))
          (t (progn
               (goto-char (point-max))
               (insert paration-string))))))

(require 'time-stamp)
(defun monologue:header-default ()
  (format "%s [%s]\n" 
          (time-stamp-string "%Y/%:m/%:d %:H:%:M")
          (or (buffer-file-name) (buffer-name)))
  )

(defvar monologue:header-function 'monologue:header-default)
(defun monologue:header ()
  (funcall monologue:header-function))

(defun monologue (message) (interactive "smessage: ")
       (let ((buf (monologue:get-current-buffer))
             (header (monologue:header)))
         (with-current-buffer buf
           (save-excursion
             (monologue:start-point)
             (insert header)
             (insert message)
             (insert "\n")
             (unless (string-match "\n$" message)
               (insert "\n"))))))


(defun my:dired-setup ()
  (define-key dired-mode-map (kbd "[") 'dired-up-directory)
  )
(add-hook 'dired-mode-hook 'my:dired-setup)

;;

;; (require 'pophint)
;; (require 'pophint-config)

;; (setq pophint:popup-max-tips 1000)
;; (setq pophint:switch-direction-p nil)
;; (setq pophint:switch-direction-char nil)
;; (setq pophint:popup-chars "hjklyuiopnmtregfd")
;; (setq pophint:select-source-chars "qazxcvbQAZXCVB")

;; (pophint:set-allwindow-command pophint:do-flexibly)

;; (pophint-config:set-automatically-when-marking nil)
;; (pophint-config:set-yank-immediately-when-marking nil)
;; (pophint-config:set-automatically-when-isearch nil)
;; (pophint-config:set-do-when-other-window nil)
;; (pophint-config:set-relayout-when-rangeyank-start nil)
;; (pophint-config:set-w3m-use-new-tab t)

;; (global-set-key (kbd "C-2")   'pophint:do)
;; (global-set-key (kbd "C-1")   'pophint:do-interactively)
;; (global-set-key (kbd "M-;")   'pophint:do-flexibly)
;; (global-set-key (kbd "C-M-;") 'pophint:redo)
;; (global-set-key (kbd "M-y")   'pophint:do-flexibly-yank)
;; (global-set-key (kbd "C-M-y") 'pophint:do-rangeyank)
;; (global-set-key (kbd "M-s S") 'pophint:do-flexibly-search)
;; (global-set-key (kbd "M-s I") 'pophint:do-flexibly-isearch)

;; (define-key dired-mode-map (kbd ";") 'pophint:do-dired-node)

;; (add-to-list 'Info-mode-hook
;;              '(lambda () (local-set-key (kbd ";") 'pophint:do-info-ref))
;;              t)

;; (add-to-list 'help-mode-hook
;;              '(lambda () (local-set-key (kbd ";") 'pophint:do-help-btn))
;;              t)

;; (add-to-list 'Custom-mode-hook
;;              '(lambda () (local-set-key (kbd ";") 'pophint:do-widget))
;;              t)



