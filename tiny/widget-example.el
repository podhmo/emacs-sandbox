(require 'widget)
(eval-when-compile (require 'wid-edit))

(defvar-keymap my:widget-field-keymap
  :parent widget-field-keymap
  :doc "個人的な拡張をしたwidget-keymap"
  "C-c C-n" #'widget-forward
  "C-c C-p" #'widget-backward
  ;; "q" #'kill-buffer
  "RET" #'newline
  "C-c n" #'my:widget-add-node
  "C-c d" #'my:widget-remove-node
  )

(setq my:node-list
      `(
        ((:tag . "memo\n") (:value . "ここに\nメモを入力"))
        ((:tag . "memo2\n") (:value . "ここにメモを入力"))
        ))


(defvar my:widget-example-buffer-name "*Widget Example*")


(defun my:widget-example (&optional node-list)
  (interactive "P")
  (setq node-list (or node-list my:node-list))
  (let ((buf (get-buffer-create my:widget-example-buffer-name)))
    (pop-to-buffer buf '(display-buffer-in-side-window (side . right) (window-width . 60)))
    (with-current-buffer buf
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (delete-all-overlays buf)
        (erase-buffer))

      (widget-insert "* example *\n\tTabとS-Tabで移動\n\n")

      (dolist (node node-list)
        (widget-create 'group
                       :indent 2
                       `(text
                         :tag ,(assoc-default :tag node)
                         :value ,(assoc-default :value node)
                         :keymap my:widget-field-keymap)
                       ))
      (use-local-map my:widget-field-keymap)
      (widget-setup)
      (widget-move 1))))

(defun my:widget-add-node ()
  (interactive)

  (save-excursion
    ;; widgetの範囲外に行く
    (goto-char (+ 1 (next-overlay-change (point))))
    (let ((inhibit-read-only t))
      (widget-create 'group
                     :indent 2
                     `(text
                       :tag "new*\n"
                       :value "<new value>"
                       :keymap my:widget-field-keymap)
                     )
      (widget-setup)))
  (widget-move 1))

(defun my:widget-remove-node ()
  (interactive)
  (let* ((w (widget-at (point)))
         (p (widget-get w :parent))) ; group
    (widget-delete (and p w))
    (widget-setup)
    (widget-move 1)))

;; serialize/deseriaize

(defun my:widget-example-to-string ()
  (with-current-buffer (get-buffer-create my:widget-example-buffer-name)
    (save-excursion
      (let ((prev (point-min))
            (buf nil))
        (goto-char (point-min))
        (while (<= prev (progn  (widget-move 1 t) (point))) ; widget-moveは終端に達したら開始地点に戻るので無限ループはしない
          (push (widget-value (widget-at (point))) buf)
          (setq prev (point)))
        (mapconcat #'identity (nreverse buf) "\n\n---\n\n")))))

(defun my:widget-example-to-clipboard ()
  (interactive)
  (kill-new (my:widget-example-to-string)))

(defun my:widget-example-from-string (text)
  (let ((node-list
         (mapcar (lambda (section) `((:tag . "memo\n") (:value . ,section)))
                 (split-string text "\n\n---\n\n" t))))
    (my:widget-example node-list)))

(defun my:widget-example-from-clipboard ()
  (interactive)
  (my:widget-example-from-string
   (with-temp-buffer
     (yank)
     (buffer-substring-no-properties (point-min) (point-max)))))


