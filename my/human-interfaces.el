;; from: https://github.com/wakaran/config/blob/master/dot.emacs.d.server/init/90-last-setting.el
(named-progn shell-settings
;;;; shell-modeで上下でヒストリ補完
  ;; C-p/C-nでヒストリを辿る (デフォルトでもM-p, M-nで出来る)
  (add-hook 'on-after-keybord-setup
            (lambda ()
              (add-hook
               'shell-mode-hook
               (function (lambda ()
                           (define-key shell-mode-map [up] 'comint-previous-input)
                           (define-key shell-mode-map [down] 'comint-next-input)
                           (define-key shell-mode-map "\C-p" 'comint-previous-input)
                           (define-key shell-mode-map "\C-n" 'comint-next-input)))))))

;; kill-ring に同じ内容の文字列を複数入れない
;; kill-ring-save 等した時にその内容が既に kill-ring にある場合、その文字列が kill-ring の先頭に 1 つにまとめられます
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;;C-gを押したときに現在の入力がヒストリーに記録されるようになる。間違ってC-gを押してしまった場合は、再び同じコマンドを起動してM-pで前の入力を呼び戻せる
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))


;; スクリプトを保存するとき()ファイルの先頭に #! が含まれているとき)，自動的に chmod +x を行う
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; 新規作成したファイルを未編集でも保存できるようにする
;; http://stackoverflow.com/questions/2592095/how-do-i-create-an-empty-file-in-emacs
(add-hook 'find-file-hooks 'assume-new-is-modified)
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

;; 連続する文末の空行を削除
(add-hook 'before-save-hook
          (lambda ()
            (save-excursion
              (goto-char (point-max))
              (delete-blank-lines))))

(named-progn popwin
  (require-and-fetch-if-not 'popwin)
  (setq display-buffer-function 'popwin:display-buffer)

  (named-progn patch ;; for bag fix
    (when (>= 1 (length (patch:function-arguments 'called-interactively-p)))
      (defun popwin:called-interactively-p ()
        (called-interactively-p))))
  )


;;; my own
(named-progn redo
  (require-and-fetch-if-not 'redo+)
  (define-many-keys (current-global-map)
    '(("C-." . redo)
      ("C-/" . undo)))
  )

(named-progn elscreen
  (require-and-fetch-if-not 'elscreen)

  (defun global-j-define-key (&optional kmap)
    (and-let* ((kmap (or kmap (current-local-map))))
      (define-key kmap "\C-j" nil))) ;; experimental

  (add-hook 'on-before-keybord-setup
            (lambda ()
              (defadvice elscreen-goto (after kill-Cj  activate)
                (global-j-define-key))
              (defadvice switch-to-buffer (after kill-Cj  activate)
                (global-j-define-key))
              (setq elscreen-prefix-key (kbd "C-j"))
              (elscreen-start)))
  )

(named-progn editing
  (require-and-fetch-if-not 'autopair)
  ;; (require-and-fetch-if-not 'paredit)
  )

(named-progn eye-candy
  (named-progn eldoc
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
               (message "named-progn -- %s --" section-name)))
            (t ad-do-it)))
    )
  )


(named-progn anything
  (require-and-fetch-if-not 'anything)
  (require-and-fetch-if-not 'anything-config)
  (require-and-fetch-if-not 'anything-match-plugin)

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

  (named-progn bookmarking-current-buffer
    (require-and-fetch-if-not 'bm)

    (defun anything-bm-list* ()
      "Preconfigured `anything' for visible bookmarks."
      (interactive)
      (with-anything-line-move-advice 
       execute-persistent-action
       (anything-bm-list)))
    )
  (named-progn popwin
    (when (boundp 'popwin:special-display-config)
      (setq anything-samewindow nil)
      (add-to-list 'popwin:special-display-config '("*anything*" :height 20))))

  (named-progn key-settings/anything
    (add-hook 'on-after-keybord-setup
              (lambda ()
                (define-many-keys global-map
                  '(("<hiragana-katakana>" . anything)
                    ("C-c C-;" . anything-occur*)
                    ("M-x" . anything-M-x)
                    ("C-x b" . anything-buffers+)
                    ("M-y" . anything-show-kill-ring)
                    
                    ("C-j C-j" . anything-bm-list*)
                    ("C-j j" . bm-toggle)
                    ))))
    )
  )  

;; (named-progn scroll-buffer 
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

(named-progn viewer-mode-settings
  (named-progn for-buffer-file-permission
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

  (named-progn keybord-settings/viewer-mode
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
