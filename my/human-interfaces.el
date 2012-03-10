(named-progn keyboad-settinsg
  (setq global-individual-key-mapping
        '(("C-c C-l" . eval-buffer)
          ("C-c C-f" . ffap)
          ("C-c C-e" . eval-defun) ;;
          ("C-c j" . dabbrev-expand)
          ("C-c C-j" . dabbrev-expand) ;;
          ("C-c q" . comment-region)
          ("C-c Q" . uncomment-region)
          
          ("C-c x" . (lambda () (interactive) (find-file (concat (current-directory) "/init.el"))))
          ("C-j S" . elscreen-shell/next-screen)

          ("C-;" . elscreen-previous)
          ("C-:" . elscreen-next)))

  (ffap-bindings) ;; url also enable when typed C-x C-f
  (define-many-keys (current-global-map) global-individual-key-mapping))

(defun elscreen-shell/next-screen () (interactive)
  (let1 dir (current-directory)
    (elscreen-create)
    (shell)
    (comint-simple-send (get-buffer-process dir)
                        (concat "cd " dir))
    (goto-char (point-max))))

;; from: https://github.com/wakaran/config/blob/master/dot.emacs.d.server/init/90-last-setting.el

;;;; shell-modeで上下でヒストリ補完
;; C-p/C-nでヒストリを辿る (デフォルトでもM-p, M-nで出来る)
(add-hook 'shell-mode-hook
          (function (lambda ()
                      (define-key shell-mode-map [up] 'comint-previous-input)
                      (define-key shell-mode-map [down] 'comint-next-input)
                      (define-key shell-mode-map "\C-p" 'comint-previous-input)
                      (define-key shell-mode-map "\C-n" 'comint-next-input))))

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
