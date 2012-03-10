(named-progn convinience-command
  (defun elscreen-shell/next-screen () (interactive)
    "create shell buffer with current directory as pwd"
    (let1 dir (current-directory)
      (elscreen-create)
      (shell)
      (comint-simple-send (get-buffer-process dir)
                          (concat "cd " dir))
      (goto-char (point-max))))

  (named-progn for-follow-mode
    (defvar split-window-function
      'split-window-horizontally)
    
    (defun follow-with-n-window (n) (interactive "p")
      "start follow-mode with n-window. n is prefix argument.
so. c-u 3 follow-with-n-window, then a frame splitted 3window
"
      (let ((wins (count-windows))
            (cur-buf (current-buffer))
            (n (if (= n 1) 2 n)))
        (when (> n wins)
          (dotimes (i (- n wins))
            (funcall split-window-function)))
        (save-window-excursion
          (dolist (w (window-list))
            (select-window w)
            (switch-to-buffer cur-buf)))
        (turn-on-follow-mode))))
  )


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
  ;; why this is need?
  (defadvice called-interactively-p (before print (&rest args) activate)
    nil)
  )


;;; my own
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

(named-progn scroll-buffer
  (require-and-fetch-if-not 'deferred)
)