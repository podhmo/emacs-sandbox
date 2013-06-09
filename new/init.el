;;; begin 0
(defun current-directory ()
  (if load-file-name
      (file-name-directory load-file-name)
      default-directory))

(add-to-list 'load-path (current-directory))


;;; begin 1
(require 'cl)
(defmacro* require-and-fetch-if-not (package &key (filename nil) (noerror t) (installed-package nil) (url nil))
  (let ((pname (gensym)))
    `(or (require ,package ,filename t)
         (let ((,pname (or ,installed-package ,package))
               (my:package-install-url ,url))
           (package-install ,pname)
           (require ,package ,filename t)))))

(require 'package)

;; dont-use, directly
(defvar my:package-install-url nil)
(defvar my:local-package-list nil)

;; custom variable 
(defvar my:local-package-sync-p t)

;; utility
(defmacro* my:package--with-work-buffer (url &rest body)
  "inspired by `package--with-work-buffer'"
  (declare (indent 1))
  (let ((buf (gensym)))
    `(let ((,buf  (url-retrieve-synchronously ,url)))
       (with-current-buffer ,buf
         (progn 
           (package-handle-response)
           (goto-char (point-min))
           (re-search-forward "^$" nil t)
           (delete-region (point-min) (+ 1 (point))))
         ,@body)
       (kill-buffer ,buf))))

(defun my:package--find-version ()
  (save-excursion
    (progn (goto-char (point-min))
           (re-search-forward "Version: *" nil t 1)
           (or (thing-at-point 'symbol) "0.0"))))

;; functions
(defun my:package-install-from-url (url name &optional version description requires)
  (let ((version version))
    (my:package--with-work-buffer url
      (unless version
        (setq version (my:package--find-version)))
      (package-unpack-single name version (or description "no description")
                             requires))
    ;; Try to activate it.
    (add-to-list 'load-path (package--dir name version))
    (when my:local-package-sync-p
      (add-to-list 'my:local-package-list (list name version))
      (my:local-package-store-save))))

;; local package
(defun my:local-package-store--create (fname &optional forcep)
  (when (or (not (file-exists-p fname)) forcep)
    (with-current-buffer (find-file-noselect fname)
      (insert "nil")
      (save-buffer))))
  
(defun my:local-package-store-fname ()
  (concat package-user-dir "/.local-package.list"))

(defun my:local-package-store-load ()
  (let ((fname (my:local-package-store-fname)))
    ;; if not found. create store file
    (my:local-package-store--create fname)
    (let ((buf (find-file-noselect fname)))
      (prog1 (read (with-current-buffer buf (buffer-string)))
        (kill-buffer buf)))))

(defun my:local-package-store-save ()
  (let ((fname (my:local-package-store-fname)))
    (with-current-buffer (find-file-noselect fname)
      (erase-buffer)
      (insert (prin1-to-string my:local-package-list))
      (save-buffer)
      (kill-buffer))))

(defun my:local-package-initialize ()
  (loop for (name version) in (my:local-package-store-load)
        do (add-to-list 'load-path (package--dir name version))))

;; advices
(defadvice package-install (around from-url-dispatch last (name) activate)
  (cond (my:package-install-url
         (let ((name (if (symbolp name) (symbol-name name) name)))
           (my:package-install-from-url my:package-install-url name)))
        (t ad-do-it)))

(defadvice package-initialize (after local-package-initialize activate)
  (my:local-package-initialize))


;;; begin 2

(defun define-many-keys (key-map key-table)
  (loop for (key . cmd) in key-table
        do (define-key key-map (read-kbd-macro key) cmd)))

;;; begin 3

(add-to-list 'load-path (current-directory))
(add-to-list 'load-path (concat (current-directory) "/3rdparty"))

(progn ;; package-management
  (require 'package)
  (setq package-user-dir (concat (current-directory) "3rdparty"))

  (progn ;; marmalade
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    ;; get available packages
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    ;; (package-list-packages)
    )) 

;;; begin 4

(progn ;; patch
  (require 'help-fns)

  (defun patch:function-arguments (sym)
    (read
     (car (help-split-fundoc (documentation sym t) sym))))
)

(defmacro tapp (exp)
  (let ((tmp (gensym)))
    `(let ((,tmp ,exp))
       (print ,tmp)
       ,tmp)))

(progn ;; hashtable
  (defsubst hash-table-get (table k &optional default)
    (gethash k table default))

  (defsubst hash-table-put (table k v)
    (puthash k v table))

  (defun hash-table-keys (table)
    (loop for k being the hash-keys in table
          collect k))

  (defun hash-table-values (table)
    (loop for v being the hash-values in table
          collect v))

  (defun hash-table->alist (table)
    (loop for k being the hash-keys in table using (hash-value v)
          collect (cons k v)))

  (defun hash-table-mapcar (fn table)
    (loop for k being the hash-keys in table using (hash-value v)
          collect (fn k v))))
   
(defmacro aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2)
           (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro and-let* (bindings &rest body)
  "imported from srfi-2"
  (declare (indent 1))
  (reduce #'(lambda (binding r)
              (let ((head (car binding)))
                (cond ((and (atom head) (symbolp head))
                       `(let (,binding)
                          (when ,head ,r)))
                      ((listp head)
                       `(when ,head ,r))
                      (t
                       (error "and-let*: invalid head %s" head)))))
          bindings :from-end t :initial-value `(progn ,@body)))


(defmacro let1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let ((,var ,val))
     ,@body))

(defmacro rlet1 (var val &rest body)
  "imported from gauche"
  (declare (indent 2))
  `(let1 ,var ,val
     ,@body
     ,var))

(defmacro with-lexical-bindings (syms &rest body)
  (declare (indent 1))
  (let ((clauses (loop for sym in syms collect (\` ((\, sym) (\, sym))))))
    (\` (lexical-let ((\,@ clauses)) (\,@ body)))))
;;;

(defmacro bind-region-or-line-with (&rest ac)
  `(multiple-value-bind (beg end)
       (if (region-active-p)
	   (values (region-beginning) (region-end))
	   (values (point-at-bol) (point-at-eol)))
     ,@ac))

(defmacro pp-mac* (macro)
  (require 'pp)
  `(let ((print-level nil)
         (print-length nil))
     (pp
      (cl-prettyexpand (quote ,macro)) (get-buffer (current-buffer)))))

(defmacro def-toggle (name &rest body)
  (and-let* ((on-clause (aif (assoc-default :on body) `(progn ,@it)))
	     (off-clause (aif (assoc-default :off body) `(progn ,@it)))
	     (state (gensym)) (flag (gensym)))
    `(lexical-let (,state)
       (defun ,name (&optional ,flag) (interactive "P")
	 (case ,flag
	   ((1 t) (progn ,on-clause (setq ,state t)))
	   ((-1) (progn ,off-clause (setq ,state nil)))
	   (otherwise (,name (if (not ,state) 1 -1))))))))


(progn ;; enclose-element
  (defun enclose-element (beg-tag end-tag)
    (multiple-value-bind (beg end)
        (if (region-active-p)
            (values (region-beginning) (region-end))
          (values (progn (skip-syntax-backward "w_") (point))
                  (progn (skip-syntax-forward "w_") (point))))
      (save-excursion
        (let1 element (buffer-substring-no-properties beg end)
          (delete-region beg end)
          (insert (format "%s%s%s" beg-tag element end-tag))))))

  (defun enclose-element-interactive (tag) (interactive "s")
    (enclose-element tag tag)))

(progn ;; delete-syntax
  (defun* delete-syntax-forward (&optional (syntax "w_"))
    (delete-region (point) (progn (skip-syntax-forward syntax) (point))))
  
  (defun delete-syntax-forward* () (interactive)
    (if (looking-at-p "[ \t]")
        (delete-region (point) (progn (skip-chars-forward "[ \t]") (point)))
      (delete-syntax-forward)))
  )
  
;;

(defun directory-files2 (directory &optional full nosort)
  (directory-files directory full "^[^\\.]\\{1,2\\}" nosort))

(defun decompose-file-path (path) ;;util
  (let ((ext  (file-name-extension path))
	(basename (file-name-nondirectory path)))
    (if ext
	(values (file-name-directory path)
		(substring basename 0 (string-match (format "\\.%s$" ext) basename))
		(concat "." ext))
	(values (file-name-directory path) basename ""))))

(defun check-target-is-exist-in-path (path target &optional find-all-p)
  (destructuring-bind (head . words) (split-string path"/")
    (let ((candidates
           (loop for word in words
                 with acc = head
                 unless (string-equal "" word)
                 do (setq acc (concat acc "/" word))
                 and collect acc into result
                 finally return (nreverse (cons head result)))))
        (cond (find-all-p
               (lexical-let ((target target))
                 (remove-if-not (lambda (path)
                                  (file-exists-p (concat path "/" target)))
                                candidates)))
              (t
               (find target candidates
                        :test (lambda (target path)
                                (file-exists-p (concat path "/" target)))))))))

(defun target-in-path (target &optional find-all-p)
  (and-let* ((dir (current-directory)))
    (check-target-is-exist-in-path
     (file-truename dir) target find-all-p)))


;;; begin 4

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(setq backup-inhibited t)
(setq max-specpdl-size 10000)
(setq max-lisp-eval-depth 10000)
(setq gc-cons-threshold (* 30 gc-cons-threshold))
(setq message-log-max 10000)
(setq history-length 1000)
(setq enable-recursive-minibuffers t)
(setq large-file-warning-threshold (* 25 1024 1024))

(add-hook 'server-switch-hook 
          (lambda ()
            (local-set-key (kbd "C-x k") 
                           (ilambda
                            (if server-buffer-clients
                                (server-edit)
                                (kill-buffer))))
            ))

(setq use-dialog-box nil)
(defalias 'message-box 'message)
(defalias  'yes-or-no-p 'y-or-n-p)

(when (boundp 'mac-key-mode)
  (mac-key-mode 1)
  (setq mac-option-modifier 'meta))

;; editing
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

(auto-compression-mode t)
(setq x-select-enable-clipboard t)

(setq comment-style 'multi-line)

;; tab
(setq-default indent-tabs-mode nil)

;;; mouse
;;(mouse-wheel-mode t)
;;(setq mouse-wheel-follow-mouse t)

(require 'font-lock)
(global-font-lock-mode t)

;;show line number
(require 'linum)
(global-linum-mode t)

;;tramp
(require 'tramp)

;; lang
(setq initial-major-mode 'emacs-lisp-mode)

;; eye candy
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(setq echo-keystrokes 0.2)
(line-number-mode t)
(column-number-mode t)
(show-paren-mode 1)
(display-time-mode t)
(transient-mark-mode t)

(setq search-highlight t)
(setq query-replace-highlight t) 

(auto-image-file-mode t)
(setq resize-mini-windows t) 

(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

;; indent
(setq default-tab-width 4)
(setq indent-line-function 'indent-relative-maybe)
(setq-default default-tab-width 4
              tab-width 4
              indent-tabs-mode nil)
;; (set-default 'tab-stop-list 
;;                (loop for i from 4 to 120 by 4
;;                      collect i))


(setq default-frame-alist
      `(
        ;; (top . 10) 
        ;; (left . 10)
      	;; (width . 106)
      	;; (height . 40)
        (frame-cursor-color . "steelblue")
        (scroll-bar-background . "grey75")
        (scroll-bar-foreground)
        (border-color . "black")
        (cursor-color . "Steelblue")
        (mouse-color . "gold")
        ;; (background-color . "black")
        (background-color . "dark slate gray")
        (foreground-color . "white")
        (font . "-apple-Menlo-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1" )
        ,@default-frame-alist))

;;(describe-face 'font-lock-variable-name-face)
;;   (print* (frame-parameters (selected-frame)))
(set-face-attribute 'font-lock-comment-face nil :weight 'semi-light :foreground "RosyBrown3")
;; (set-face-attribute 'help-argument-name nil :slant 'normal)
(set-face-attribute 'font-lock-string-face nil :foreground "RosyBrown3" :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :foreground "PaleGreen3" :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :foreground "SlateBlue3" :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil :foreground "plum3" :slant 'normal :weight 'bold)
(set-face-attribute 'font-lock-function-name-face nil :foreground "plum3" :weight 'bold)
(set-face-attribute 'font-lock-type-face nil :foreground "DarkSlateGray4":weight 'bold)



;;; begin 5

(defun open-shell-with-pwd () (interactive)
  (let1 dir (current-directory)
    (shell)
    (comint-simple-send (get-buffer-process dir)
                        (concat "cd " dir))
    (goto-char (point-max))))

(defun current-hook-change-to-empty () (interactive)
  (let ((hook (symbol-at-point)))
    (cond ((boundp hook)
           (unwind-protect
               (progn
                 (describe-variable hook)
                 (when (yes-or-no-p (format "%s -> nil? :" hook))
                   (eval `(setq ,hook nil))
                   (message "%s -> nil!" hook)))
             (dolist (w (get-buffer-window-list (help-buffer)))
               (delete-window  w))))
          (t (message "no bound found -%s-" hook)))))

(progn ;; for-follow-mode
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


(progn ;; treat-dumped-junks
  (defvar junks-directory-path "~/junks")
  (defvar junks-directory-force-create-p t)

  (defun junks-create-directory-if-force (force-p)
    (unless (and force-p
                 (file-exists-p junks-directory-path) 
                 (file-directory-p junks-directory-path))
      (make-directory junks-directory-path)))

  (defun junks-insert-content (strings)
    (save-excursion
      (goto-char (point-max))
      (insert "\n\n")
      (dolist (s strings)
        (insert s "\n"))))

  (defun move-junks (&rest contents) 
    (let* ((timestamp (format-time-string "%Y-%m-%d" (current-time)))
           (fname (format "%s/junks.%s" junks-directory-path timestamp)))
      (junks-create-directory-if-force  junks-directory-force-create-p)
      (with-current-buffer (find-file-noselect fname)
        (junks-insert-content contents))))

  (defun move-junks-region (beg end comment) (interactive "r\nscomment:")
    (move-junks
     comment
     (delete-and-extract-region beg end)))
  )


;;; begin 6
;; from: https://github.com/wakaran/config/blob/master/dot.emacs.d.server/init/90-last-setting.el
(progn ;; shell-settings
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
  (require-and-fetch-if-not 'popwin)
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
  )


;;; my own
(progn ;; redo
  (require-and-fetch-if-not 'redo+)
  )
(progn ;; zlc
  (require-and-fetch-if-not 'zlc)
  )

;; (progn ;; tabbar
;;   ;; ("C-;" . my:tabbar-backward-tab)
;;   ;; ("C-:" . my:tabbar-forward-tab)
;;   ;; ("C-j p" . my:tabbar-backward-tab)
;;   ;; ("C-j n" . my:tabbar-forward-tab)
;;   ;; ("C-j C-p" . my:tabbar-backward-tab)
;;   ;; ("C-j C-n" . my:tabbar-forward-tab)
;;   ;; ("C-j c" . my:tabbar-create)
;;   ;; ("C-j C-k" . my:tabbar-hidden-tab)

;;   (require-and-fetch-if-not 'tabbar)
;;   (tabbar-mode 1)
;;   (setq tabbar-buffer-groups-function nil)

;;   (defvar my:always-display-buffers '("*scratch*" "*shell*"))
;;   (defvar my:hidden-buffers-list nil)
;;   (defadvice switch-to-buffer (after pop-switched-buffer-from-hidden-list activate)
;;     (setq my:hidden-buffers-list (delete (current-buffer) my:hidden-buffers-list)))

;;   (defun my:tabbar-buffer-list ()
;;     (append (loop for bname in my:always-display-buffers
;;                   for b = (get-buffer bname)
;;                   when b collect b)
;;             (loop for b in (buffer-list)
;;                   when (and (buffer-file-name b) (not (member b my:hidden-buffers-list)))
;;                   collect b)))

;;   (defvar my:tabbar-configuration-table (make-hash-table :test 'equal)) 
;;   (defun my:tabbar--around (action)
;;     "almost same defadvice's around + ad-do-it"
;;     (lexical-let ((has-many-windows (> (count-windows) 1)))
;;       (when has-many-windows
;;         (hash-table-put my:tabbar-configuration-table
;;                         (current-buffer) (current-window-configuration)))
;;       (funcall action)
;;       (or (and-let* ((wconf (hash-table-get my:tabbar-configuration-table
;;                                             (current-buffer) nil)))
;;             (set-window-configuration wconf))
;;           (and has-many-windows
;;                (delete-other-windows)))))

;;   (defun my:tabbar-hidden-tab (&optional b) (interactive)
;;     (let ((b (or b (current-buffer))))
;;       (my:tabbar--around 'tabbar-forward-tab)
;;       (add-to-list 'my:hidden-buffers-list b)))
;;   (tabbar-backward-tab)
;;   (defun my:tabbar-forward-tab () (interactive)
;;     (my:tabbar--around 'tabbar-forward-tab))
;;   (defun my:tabbar-backward-tab () (interactive)
;;     (my:tabbar--around 'tabbar-backward-tab))
;;   (defun my:tabbar-create () (interactive)
;;     (my:tabbar--around (lambda () (switch-to-buffer "*scratch*"))))

;;   (setq tabbar-buffer-list-function 'my:tabbar-buffer-list)
;;   (setq tabbar-separator '(0.5))
  
;;   (add-hook 'on-before-keybord-setup
;;             (lambda ()
;;               (defun global-j-define-key (&optional kmap)
;;                 (and-let* ((kmap (or kmap (current-local-map))))
;;                   (define-key kmap "\C-j" nil))) ;; experimental
              
;;               (defadvice elscreen-goto (after kill-Cj  activate)
;;                 (global-j-define-key))

;;               (defadvice switch-to-buffer (after kill-Cj  activate)
;;                 (global-j-define-key))))

;;   (add-hook 'after-init-hook
;;             (lambda ()
;;               (set-face-attribute
;;                'tabbar-unselected nil
;;                :background (frame-parameter (selected-frame) 'background-color)
;;                :foreground (frame-parameter (selected-frame) 'foreground-color)
;;                :box nil)
;;               (set-face-attribute
;;                'tabbar-selected nil
;;                :background (frame-parameter (selected-frame) 'background-color)
;;                :foreground "yellow"
;;                :box nil)
;;               (set-face-attribute
;;                'tabbar-button nil
;;                :box nil)
;;               (set-face-attribute
;;                'tabbar-separator nil
;;                :height 1.5)
;;               ))
;;   )

(progn ;; editing
  (require-and-fetch-if-not 'autopair)
  (setq-default autopair-dont-pair `(:string (?') :comment  (?') :never (?`)))
  ;; (require-and-fetch-if-not 'paredit)
  )

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
  (require-and-fetch-if-not 'anything)
  (require-and-fetch-if-not 'anything-config)
  (require-and-fetch-if-not 'anything-match-plugin)
  ;; (require-and-fetch-if-not 'anything-complete)

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
    (require-and-fetch-if-not 'bm)

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
    (add-hook 'on-after-keybord-setup
              (lambda ()
                (define-many-keys global-map
                  '(("<hiragana-katakana>" . anything)
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

  (progn ;; keybord-settings/viewer-mode
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
     ;; (require-and-fetch-if-not 'pym :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/pym.el")     
     ;; (require-and-fetch-if-not 'apel-ver :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/apel-ver.el")
     ;; (require-and-fetch-if-not 'product :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/product.el")
     ;; (require-and-fetch-if-not 'static :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/static.el")
     ;; (require-and-fetch-if-not 'alist :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/alist.el")
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
   )
;; from: https://github.com/wakaran/config/blob/master/dot.emacs.d.server/init/90-last-setting.el
(progn ;; shell-settings
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
  (require-and-fetch-if-not 'popwin)
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
  )


;;; my own
(progn ;; redo
  (require-and-fetch-if-not 'redo+)
  )
(progn ;; zlc
  (require-and-fetch-if-not 'zlc)
  )

;; (progn ;; tabbar
;;   ;; ("C-;" . my:tabbar-backward-tab)
;;   ;; ("C-:" . my:tabbar-forward-tab)
;;   ;; ("C-j p" . my:tabbar-backward-tab)
;;   ;; ("C-j n" . my:tabbar-forward-tab)
;;   ;; ("C-j C-p" . my:tabbar-backward-tab)
;;   ;; ("C-j C-n" . my:tabbar-forward-tab)
;;   ;; ("C-j c" . my:tabbar-create)
;;   ;; ("C-j C-k" . my:tabbar-hidden-tab)

;;   (require-and-fetch-if-not 'tabbar)
;;   (tabbar-mode 1)
;;   (setq tabbar-buffer-groups-function nil)

;;   (defvar my:always-display-buffers '("*scratch*" "*shell*"))
;;   (defvar my:hidden-buffers-list nil)
;;   (defadvice switch-to-buffer (after pop-switched-buffer-from-hidden-list activate)
;;     (setq my:hidden-buffers-list (delete (current-buffer) my:hidden-buffers-list)))

;;   (defun my:tabbar-buffer-list ()
;;     (append (loop for bname in my:always-display-buffers
;;                   for b = (get-buffer bname)
;;                   when b collect b)
;;             (loop for b in (buffer-list)
;;                   when (and (buffer-file-name b) (not (member b my:hidden-buffers-list)))
;;                   collect b)))

;;   (defvar my:tabbar-configuration-table (make-hash-table :test 'equal)) 
;;   (defun my:tabbar--around (action)
;;     "almost same defadvice's around + ad-do-it"
;;     (lexical-let ((has-many-windows (> (count-windows) 1)))
;;       (when has-many-windows
;;         (hash-table-put my:tabbar-configuration-table
;;                         (current-buffer) (current-window-configuration)))
;;       (funcall action)
;;       (or (and-let* ((wconf (hash-table-get my:tabbar-configuration-table
;;                                             (current-buffer) nil)))
;;             (set-window-configuration wconf))
;;           (and has-many-windows
;;                (delete-other-windows)))))

;;   (defun my:tabbar-hidden-tab (&optional b) (interactive)
;;     (let ((b (or b (current-buffer))))
;;       (my:tabbar--around 'tabbar-forward-tab)
;;       (add-to-list 'my:hidden-buffers-list b)))
;;   (tabbar-backward-tab)
;;   (defun my:tabbar-forward-tab () (interactive)
;;     (my:tabbar--around 'tabbar-forward-tab))
;;   (defun my:tabbar-backward-tab () (interactive)
;;     (my:tabbar--around 'tabbar-backward-tab))
;;   (defun my:tabbar-create () (interactive)
;;     (my:tabbar--around (lambda () (switch-to-buffer "*scratch*"))))

;;   (setq tabbar-buffer-list-function 'my:tabbar-buffer-list)
;;   (setq tabbar-separator '(0.5))
  
;;   (add-hook 'on-before-keybord-setup
;;             (lambda ()
;;               (defun global-j-define-key (&optional kmap)
;;                 (and-let* ((kmap (or kmap (current-local-map))))
;;                   (define-key kmap "\C-j" nil))) ;; experimental
              
;;               (defadvice elscreen-goto (after kill-Cj  activate)
;;                 (global-j-define-key))

;;               (defadvice switch-to-buffer (after kill-Cj  activate)
;;                 (global-j-define-key))))

;;   (add-hook 'after-init-hook
;;             (lambda ()
;;               (set-face-attribute
;;                'tabbar-unselected nil
;;                :background (frame-parameter (selected-frame) 'background-color)
;;                :foreground (frame-parameter (selected-frame) 'foreground-color)
;;                :box nil)
;;               (set-face-attribute
;;                'tabbar-selected nil
;;                :background (frame-parameter (selected-frame) 'background-color)
;;                :foreground "yellow"
;;                :box nil)
;;               (set-face-attribute
;;                'tabbar-button nil
;;                :box nil)
;;               (set-face-attribute
;;                'tabbar-separator nil
;;                :height 1.5)
;;               ))
;;   )

(progn ;; editing
  (require-and-fetch-if-not 'autopair)
  (setq-default autopair-dont-pair `(:string (?') :comment  (?') :never (?`)))
  ;; (require-and-fetch-if-not 'paredit)
  )

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
  (require-and-fetch-if-not 'anything)
  (require-and-fetch-if-not 'anything-config)
  (require-and-fetch-if-not 'anything-match-plugin)
  ;; (require-and-fetch-if-not 'anything-complete)

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
    (require-and-fetch-if-not 'bm)

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
    (add-hook 'on-after-keybord-setup
              (lambda ()
                (define-many-keys global-map
                  '(("<hiragana-katakana>" . anything)
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

  (progn ;; keybord-settings/viewer-mode
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
     ;; (require-and-fetch-if-not 'pym :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/pym.el")     
     ;; (require-and-fetch-if-not 'apel-ver :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/apel-ver.el")
     ;; (require-and-fetch-if-not 'product :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/product.el")
     ;; (require-and-fetch-if-not 'static :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/static.el")
     ;; (require-and-fetch-if-not 'alist :url "https://raw.github.com/nelhage/elisp/master/site/apel-10.6/alist.el")
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
   )

;;; begin 7

;;; debug utiltities
(defvar cmh:debug-buffer nil)
(defvar cmh:me nil)
(defvar cmh:debug-buffer-name "*cmh:debug*")
(defvar cmh:is-display-when-debug-log t)
(defmacro cmh:with-debug-buffer (&rest body)
  `(progn
     (unless cmh:debug-buffer
       (setq cmh:debug-buffer (get-buffer-create cmh:debug-buffer-name)))
     (with-current-buffer cmh:debug-buffer
       ,@body)))

(defun cmh:debug-message-default ()
  (let1 time (format-time-string "/%Y/%m/%d %H:%M:%S" (current-time))
    (format "%s mode: (cmh:%s, %s) buffer:(%s)\n"
            time cmh:current-major-mode major-mode
            (current-buffer))))

(defvar cmh:debug-message-function 'cmh:debug-message-default)

(defun* cmh:debug-current-state (&optional (prefix "") (suffix ""))
  (let1 message (funcall cmh:debug-message-function)
    (cmh:with-debug-buffer
     (goto-char (point-max))
     (insert prefix message suffix)))
  (when cmh:is-display-when-debug-log
    (display-buffer cmh:debug-buffer)))

(defun cmh:debug-current-state* ()
  (cmh:debug-current-state (concat cmh:me "\t")))

(defun change-mode-hook-debug-activate () (interactive)
  (add-hook 'cmh:change-mode-hook 'cmh:debug-current-state*))

(defun change-mode-hook-debug-deactivate () (interactive)
  (remove-hook 'cmh:change-mode-hook 'cmh:debug-current-state*))

;;; code:
(defvar cmh:change-mode-hook nil)
(defvar cmh:current-major-mode nil)
;; (defvar cmh:exclude-major-mode-list '(lisp-interaction-mode))

(defadvice other-window (around cmh:hook-other-window activate)
  (setq cmh:me "other-window")
  ad-do-it
  (unless (eq cmh:current-major-mode major-mode)
    (run-hooks 'cmh:change-mode-hook)
    (setq cmh:current-major-mode major-mode)))

;;; switch-to-buffer and set-window-configuration are too noizy.
;; (defadvice switch-to-buffer (around cmh:hook-switch-to-buffer activate)
;;   (setq cmh:me "switch-to-buffer")
;;   (setq cmh:current-major-mode major-mode)
;;   ad-do-it
;;   (unless (eq cmh:current-major-mode major-mode)
;;     (run-hooks 'cmh:change-mode-hook)
;;     (setq cmh:current-major-mode major-mode)))
;; (defadvice set-window-configuration (around cmh:hook activate)
;;   (when (and (equal chm:current-buffer (current-buffer))
;;              (not (eq cmh:current-major-mode major-mode)))
;;     (run-hooks 'cmh:before-change-mode-hook))
;;   ad-do-it
;;   (when (and (equal chm:current-buffer (current-buffer))
;;              (not (eq cmh:current-major-mode major-mode)))
;;     (run-hooks 'cmh:before-change-mode-hook)
;;     (setq cmh:current-major-mode major-mode)))

(defadvice find-file (around cmh:hook-find-file activate)
  (setq cmh:me "find-file")
  (setq cmh:current-major-mode major-mode)
  ad-do-it
  (unless (eq cmh:current-major-mode major-mode)
    (run-hooks 'cmh:change-mode-hook)
    (setq cmh:current-major-mode major-mode)))

(when (fboundp 'elscreen-goto)
  (defadvice elscreen-goto (around cmh:hook-elscreen-goto activate)
    (setq cmh:me "elscreen-goto")
    (setq cmh:current-major-mode major-mode)
    ad-do-it
    (unless (eq cmh:current-major-mode major-mode)
      (run-hooks 'cmh:change-mode-hook)
      (setq cmh:current-major-mode major-mode))))

;; (change-mode-hook-debug-activate)
  (add-hook 'after-init-hook
            (lambda ()
              (keybord-settings-setup)
              ))

;;; begin 7

(require-and-fetch-if-not 'anything)

(progn ;; keyboad-settings
  (progn ;; key-chord
    (require-and-fetch-if-not 'key-chord :url "http://www.emacswiki.org/emacs/download/key-chord.el")
    (setq key-chord-two-keys-delay 0.01)
    (key-chord-mode 1)
    )

  (defvar on-after-keybord-setup (list))
  (defvar on-before-keybord-setup (list))
  (defmacro with-before-keybord-setup (&rest body)
    `(add-hook 
      'on-before-keybord-setup
      (lambda ()
        ,@body)))
  (defmacro with-after-keybord-setup (&rest body)
    `(add-hook 
     'on-after-keybord-setup
     (lambda ()
       ,@body)))

  (defvar ctl-j-map (make-keymap))
  (global-set-key (kbd "C-j") ctl-j-map)
  
  (defvar keybord-settings:curdir (current-directory))
  (defun keybord-settings-setup ()
    ;; occur before settings hook
    (run-hook-with-args-until-failure
     'on-before-keybord-setup)

    (progn ;; global-key-settings ;; or human-interfaces.el
      (setq global-individual-key-mapping
            '(("C-c C-l" . eval-buffer)
              ("M-r" . replace-string)
              ("M-R" . replace-regexp)

              ("C-x C-l" . goto-line)

              ("C-c C-c" . toggle-file)
              ("C-c C-f" . ffap)
              ("C-c C-e" . eval-defun) ;;
              ("C-c j" . dabbrev-expand)
              ("C-c C-j" . dabbrev-expand) ;;
              ("C-c q" . comment-region)
              ("C-c Q" . uncomment-region)
              
              ("C-c x" . (lambda () (interactive) (find-file (concat keybord-settings:curdir "/init.el"))))
              ("C-c e" . enclose-element-interactive)
              ("C-c d" . delete-syntax-forward*)
              ("M-r" . replace-string)
              ("M-R" . replace-regexp)

              ;; quick-run
              ("C-c @" . quickrun-compile-only)
              ("C-c C-@" . quickrun)

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

    (ffap-bindings) ;; url also enable when typed C-x C-f

    (progn ;; key-chord
      (key-chord-define-global "jk" 'view-mode)
      (key-chord-define-global "po" 'org-remember))

    ;; occur after settings hook
    (run-hook-with-args-until-failure
     'on-after-keybord-setup)
    ))

(progn ;; anything-git
  (require-and-fetch-if-not 'with-prefix :url "https://raw.github.com/podhmo/anything-vcs-project.el/master/with-prefix.el")
  (require-and-fetch-if-not 'anything-vcs-project :url "https://raw.github.com/podhmo/anything-vcs-project.el/master/anything-vcs-project.el")
  (setq anything-vcs-project:cache-enable-p t)
  (setq anything-vcs-project:cache.project-list-path "~/.emacs.d/.project.list")
  (setq anything-vcs-project-git:exclude-args "--exclude .hg --exclude '*.pyc'")
  (setq anything-vcs-project-hg:exclude-args "--exclude .git --exclude '*.pyc'")
  (global-set-key (kbd "C-c C-:") 'anything-vcs-project)
)

(progn ;; emacsclient
  ;; emacsclient サーバを起動
  (condition-case err
      (progn
        (autoload 'server-running-p "server") 
        (unless (server-running-p)  (server-start)))
    (error (message "emacsclient load fail"))))

(progn ;; recentf
  (setq recentf-max-saved-items 500)
  (recentf-mode 1))

(progn ;; uniquify
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(progn ;; toggle-file-mode
  (require-and-fetch-if-not 'toggle-file-mode :url "https://raw.github.com/gist/2600353/9a5d6965bc075c2c967b8fbb832cc4800abc14dc/toggle-file-mode.el"))
  
;; (progn ;; speedbar
;;   (require-and-fetch-if-not 'sr-speedbar)
;;   )

;; code-block
(require-and-fetch-if-not 'config-block :url "https://gist.github.com/podhmo/3917706/raw/3bd74df6fbc69995be57f4635d4b14f2afeedaa4/config-block.el")

;; python
;; (load "language.python-new")
;; (config-block-setup! 
;;  '(
;;    addon:python.core
;;    addon:python.strict.indent
;;    addon:python.find-python
;;    addon:python.import.ffap
;;    addon:python.quickrun
;;    addon:python.hook.initialize
;;    ))

(progn ;; programming-languages
  (progn ;; quick-run
    (require-and-fetch-if-not 'quickrun :url "https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el")
    (defadvice quickrun (around help-mode-after-quickrun activate)
      (lexical-let ((before-buf (current-buffer)))
        ad-do-it
        (when (and (not (equal before-buf (current-buffer)))
                   (eq major-mode 'fundamental-mode))
          (help-mode))))
    )

  (progn ;; font-lock-language-plugin
    ;; (defmacro language:define-plugin (name args &rest body)
    ;;   (declare (indent 2))
    ;;   `(defun* ,name ,args 
    ;;      (add-to-list 'language:activated-plugins ',name)
    ;;      ,@body))
    (font-lock-add-keywords
     'emacs-lisp-mode 
     '(("(\\([^\t ]*?:define-plugin\\) " (1 font-lock-keyword-face) 
        ("[^\t ]+?" nil nil (0 font-lock-function-name-face))))))
  
  (progn ;; emacs-lisp
    (require-and-fetch-if-not 'paredit)
    (define-key paredit-mode-map (kbd "C-j") ctl-j-map)

    (defun my:emacs-lisp-setup ()
      (define-many-keys emacs-lisp-mode-map
        '(("C-c C-j" . lisp-complete-symbol)
          ("C-c M-r" . paredit-forward-slurp-sexp)
          ("C-c M-R" . paredit-forward-barf-sexp)
          ("C-c M-l" . paredit-backward-slurp-sexp)
          ("C-c M-L" . paredit-backward-barf-sexp)
          ))
      (turn-on-eldoc-mode)
      (eldoc-add-command
       'paredit-backward-delete
       'paredit-close-round)
      (paredit-mode +1)
      )    
    (add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-setup))

  ;; (progn ;; yasnipet ;;move-it
  ;;   (require-and-fetch-if-not 'yasnippet)
  ;;   (yas/load-directory (concat (current-directory) "3rdparty/yasnippet-20120320/snippets")))
  
  (progn ;; auto-complete ;;move-it
    (require-and-fetch-if-not 'auto-complete)
    (setq ac-use-menu-map t)
    (define-key ac-menu-map "\C-n" 'ac-next)
    (define-key ac-menu-map "\C-p" 'ac-previous)
    )

  (progn ;; html
    (add-hook 'html-mode-hook ;; move-it
              (lambda ()
                (autopair-on)
                (modify-syntax-entry ?% "w_"))))
  (progn ;; scheme

(add-hook 'inferior-scheme-mode-hook
	  (lambda () (ansi-color-for-comint-mode-on)))

 (defun scheme-insert-closing (prefix default-close other-open other-close) 
   (insert default-close) 
   (unless prefix 
     (let ((open-pt (condition-case nil 
			(scan-sexps (point) -1) 
		      (error (beep) nil)))) 
       (when open-pt 
	 (let ((open-char (aref (buffer-substring-no-properties
				 open-pt (1+ open-pt))
				0))) 
	   (when (= open-char other-open) 
	     (delete-backward-char 1) 
	     (insert other-close)))))))

(defun scheme-insert-closing-paren (&optional prefix) 
  (interactive "P") 
  (scheme-insert-closing prefix ?\) ?\[ ?\]))  

(setq scheme-keybind
      '((")" . scheme-insert-closing-paren) 
    ;;     ("C-c C-l" . (lambda () (interactive) 
    ;;                     (let ((file (buffer-file-name)))
    ;;                       (if file 
    ;;                           (scheme-load-file file)
    ;;                         (scheme-send-region (point-min) (point-max))))))
	;; ("C-c S" . scheme-other-window)
	;; ("C-c C-S" . scheme-other-frame)
	;;("\C-c\C-k" . scheme-kill-repl)
        ))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?@ "w" scheme-mode-syntax-table)
	    (modify-syntax-entry ?| "w" scheme-mode-syntax-table)
	    (define-many-keys scheme-mode-map scheme-keybind)
        ))

(defmacro set-put (progname edit-list)
  "edit-list = ((<value> . (sym ...)) ...)"
  `(progn
     ,@(loop for (depth sym-list) in edit-list nconc
	     (loop for sym in sym-list collect `(put (quote ,sym) ,progname ,depth)))))

(set-put 'scheme-indent-function
	 ((1
	   (guard with-locking-mutex with-signal-handlers with-time-counter with-string-io with-port-locking with-output-to-string with-output-to-process with-output-to-port with-output-conversion with-module with-iterator with-input-from-string with-input-from-process with-input-from-port with-input-conversion with-error-to-port with-builder while when until unless syntax-rules rxmatch-case parse-options parameterize match make letrec-syntax let/cc let-values let-syntax let*-values dotimes dolist call-with-values call-with-temporary-file call-with-output-file call-with-output-conversion call-with-iterator call-with-input-string call-with-input-process call-with-input-file call-with-input-conversion call-with-client-socket and-let* port-fold port-fold-right))
	  (2 
	   (rxmatch-let rxmatch-if receive multiple-value-bind match-let1 let1 let-optionals* let-match let-keywords let-keywords* let-args if-match))
	  (0 
	   (with-error-handler rxmatch-cond call-with-output-string begin0))))

(add-hook 'inferior-scheme-mode-hook
	  (lambda () (ansi-color-for-comint-mode-on)))

 (defun scheme-insert-closing (prefix default-close other-open other-close) 
   (insert default-close) 
   (unless prefix 
     (let ((open-pt (condition-case nil 
			(scan-sexps (point) -1) 
		      (error (beep) nil)))) 
       (when open-pt 
	 (let ((open-char (aref (buffer-substring-no-properties
				 open-pt (1+ open-pt))
				0))) 
	   (when (= open-char other-open) 
	     (delete-backward-char 1) 
	     (insert other-close)))))))

(defun scheme-insert-closing-paren (&optional prefix) 
  (interactive "P") 
  (scheme-insert-closing prefix ?\) ?\[ ?\]))  

(setq scheme-keybind
      '((")" . scheme-insert-closing-paren) 
    ;;     ("C-c C-l" . (lambda () (interactive) 
    ;;                     (let ((file (buffer-file-name)))
    ;;                       (if file 
    ;;                           (scheme-load-file file)
    ;;                         (scheme-send-region (point-min) (point-max))))))
	;; ("C-c S" . scheme-other-window)
	;; ("C-c C-S" . scheme-other-frame)
	;;("\C-c\C-k" . scheme-kill-repl)
        ))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?@ "w" scheme-mode-syntax-table)
	    (modify-syntax-entry ?| "w" scheme-mode-syntax-table)
	    (define-many-keys scheme-mode-map scheme-keybind)
        ))

(defmacro set-put (progname edit-list)
  "edit-list = ((<value> . (sym ...)) ...)"
  `(progn
     ,@(loop for (depth sym-list) in edit-list nconc
	     (loop for sym in sym-list collect `(put (quote ,sym) ,progname ,depth)))))

(set-put 'scheme-indent-function
	 ((1
	   (guard with-locking-mutex with-signal-handlers with-time-counter with-string-io with-port-locking with-output-to-string with-output-to-process with-output-to-port with-output-conversion with-module with-iterator with-input-from-string with-input-from-process with-input-from-port with-input-conversion with-error-to-port with-builder while when until unless syntax-rules rxmatch-case parse-options parameterize match make letrec-syntax let/cc let-values let-syntax let*-values dotimes dolist call-with-values call-with-temporary-file call-with-output-file call-with-output-conversion call-with-iterator call-with-input-string call-with-input-process call-with-input-file call-with-input-conversion call-with-client-socket and-let* port-fold port-fold-right))
	  (2 
	   (rxmatch-let rxmatch-if receive multiple-value-bind match-let1 let1 let-optionals* let-match let-keywords let-keywords* let-args if-match))
	  (0 
	   (with-error-handler rxmatch-cond call-with-output-string begin0))))

    (load "language.scheme"))

  (progn ;; python 
    (load "language.python")

    (progn ;; activate-plugin
      (python:auto-mode-alist-plugin)
      (python:flymake-plugin)
      (python:autopair-plugin)
      (python:strict-indent-plugin)
      (python:flymake-eldoc/current-position-plugin)
      (python:anything-with-modules-plugin)
      ;; (python:yasnippet-plugin)
      ;; (python:run-program-plugin-simple)
      (python:quickrun-with-virtualenv-plugin)
      (python:auto-complete-plugin)
      (python:swap-backquote-and-underscore-plugin)
      )

    (defun my:python-setup ()
      (run-hooks 'python:plugin-mode-hook)

      (let1 keymaps
          (loop for (plugin . key-maps) in
                '((python:run-program-plugin-simple
                   . (("C-c @" . python:run-program-current-buffer)))
                  (python:anything-with-modules-plugin
                   . (("C-c C-f" . python:anything-with-modules)
                      ("C-c f" . python-describe-symbol))))
                when (python:plugin-activate-p plugin)
                append key-maps)
        (define-many-keys python-mode-map keymaps)
        (define-key python-mode-map "\C-c\C-c" 'toggle-file)
        ))

    (add-hook 'python-mode-hook 'my:python-setup))

  (progn ;; ruby
    (require-and-fetch-if-not 'flymake-ruby)
    (require 'ruby-mode nil t)
    (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  )
)

(run-hook-with-args 'after-init-hook)

(defadvice anything-M-x (before keep-history-size-more-than-one activate)
    (while (null (cdr extended-command-history))
      (push "*dummy*" extended-command-history)))

;; this is ad-hoc settings for mac
(define-key global-map "¥" (lambda (&optional n) (interactive "p") (dotimes (i (or n 1))  (insert "\\"))))
(setq-default tab-stop-list (loop for i from 4 to 120 by 4
                                  collect i))

(setq-default ring-bell-function
              (lambda () (message "ding")))

;; this is haskell settings
;; sudo port install ghc hs-cabal
;; cabal-0.14.0 update
;; cabal-0.14.0 install cabal-install
;; cabal-0.14.0 install ghc-mod hlint
(require-and-fetch-if-not 'haskell-mode)
(require-and-fetch-if-not 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) 

(defun my:haskell-cabal-home ()
  (concat (getenv "HOME") "/.cabal"))

(add-to-list 'exec-path (concat (my:haskell-cabal-home) "/bin"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(org-agenda-files (quote ("~/org/sample.org")))
 '(remember-handler-functions (quote (remember-insert-top-of-the-file))))

(defun my:haskell-setup ()
  (ghc-init)
  (flymake-mode)
  )
(add-hook 'haskell-mode-hook 'my:haskell-setup)

(add-to-list 'load-path (concat (current-directory) "ghc-mod"))
(autoload 'ghc-init "ghc" nil t)

(put 'narrow-to-region 'disabled nil)
(setq debug-on-error nil)

(defun simple-timer (n d &optional color) (interactive "nwait
ndelay")
  (run-with-timer 
   n nil 
   (lexical-let ((d d) (color (or color "#8d8d8d")))
     (lambda (&rest args)
       (lexical-let ((original-color (background-color-at-point)))
         (set-background-color color)
         (run-with-timer d nil (lambda (&rest args) (set-background-color original-color)))
         )))))

;; org-mode
(require 'org-install)
(require 'org-remember)
(add-to-list 'auto-mode-alist '("\\.notes$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("memo[0-9]+\\.txt" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun remember-insert-top-of-the-file ()
  (let ((text (buffer-string))
        (desc (remember-buffer-desc)))
    (with-temp-buffer
      (insert remember-leader-text (current-time-string)
              " (" desc ")\n\n" text)
      (let ((remember-text (buffer-string)))
        (with-current-buffer (find-file-noselect remember-data-file)
          (save-excursion
            (goto-char (point-min))
            (insert remember-text "\n")
            (goto-char (point-min))
            (when remember-save-after-remembering (save-buffer)))))
      )))
(setq remember-handler-functions '(remember-insert-top-of-the-file))

;; org-agenda
(setq calendar-holidays nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
(setq org-log-done 'time)

(setq org-agenda-custom-commands
      '(("f" occur-tree "FIXME")))

(require-and-fetch-if-not 'anything)

(progn ;; keyboad-settings
  (progn ;; key-chord
    (require-and-fetch-if-not 'key-chord :url "http://www.emacswiki.org/emacs/download/key-chord.el")
    (setq key-chord-two-keys-delay 0.01)
    (key-chord-mode 1)
    )

  (defvar on-after-keybord-setup (list))
  (defvar on-before-keybord-setup (list))
  (defmacro with-before-keybord-setup (&rest body)
    `(add-hook 
      'on-before-keybord-setup
      (lambda ()
        ,@body)))
  (defmacro with-after-keybord-setup (&rest body)
    `(add-hook 
     'on-after-keybord-setup
     (lambda ()
       ,@body)))

  (defvar ctl-j-map (make-keymap))
  (global-set-key (kbd "C-j") ctl-j-map)
  
  (defvar keybord-settings:curdir (current-directory))
  (defun keybord-settings-setup ()
    ;; occur before settings hook
    (run-hook-with-args-until-failure
     'on-before-keybord-setup)

    (progn ;; global-key-settings ;; or human-interfaces.el
      (setq global-individual-key-mapping
            '(("C-c C-l" . eval-buffer)
              ("M-r" . replace-string)
              ("M-R" . replace-regexp)

              ("C-x C-l" . goto-line)

              ("C-c C-c" . toggle-file)
              ("C-c C-f" . ffap)
              ("C-c C-e" . eval-defun) ;;
              ("C-c j" . dabbrev-expand)
              ("C-c C-j" . dabbrev-expand) ;;
              ("C-c q" . comment-region)
              ("C-c Q" . uncomment-region)
              
              ("C-c x" . (lambda () (interactive) (find-file (concat keybord-settings:curdir "/init.el"))))
              ("C-c e" . enclose-element-interactive)
              ("C-c d" . delete-syntax-forward*)
              ("M-r" . replace-string)
              ("M-R" . replace-regexp)

              ;; quick-run
              ("C-c @" . quickrun-compile-only)
              ("C-c C-@" . quickrun)

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

    (ffap-bindings) ;; url also enable when typed C-x C-f

    (progn ;; key-chord
      (key-chord-define-global "jk" 'view-mode)
      (key-chord-define-global "po" 'org-remember))

    ;; occur after settings hook
    (run-hook-with-args-until-failure
     'on-after-keybord-setup)
    ))

(progn ;; anything-git
  (require-and-fetch-if-not 'with-prefix :url "https://raw.github.com/podhmo/anything-vcs-project.el/master/with-prefix.el")
  (require-and-fetch-if-not 'anything-vcs-project :url "https://raw.github.com/podhmo/anything-vcs-project.el/master/anything-vcs-project.el")
  (setq anything-vcs-project:cache-enable-p t)
  (setq anything-vcs-project:cache.project-list-path "~/.emacs.d/.project.list")
  (setq anything-vcs-project-git:exclude-args "--exclude .hg --exclude '*.pyc'")
  (setq anything-vcs-project-hg:exclude-args "--exclude .git --exclude '*.pyc'")
  (global-set-key (kbd "C-c C-:") 'anything-vcs-project)
)

(progn ;; emacsclient
  ;; emacsclient サーバを起動
  (condition-case err
      (progn
        (autoload 'server-running-p "server") 
        (unless (server-running-p)  (server-start)))
    (error (message "emacsclient load fail"))))

(progn ;; recentf
  (setq recentf-max-saved-items 500)
  (recentf-mode 1))

(progn ;; uniquify
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(progn ;; toggle-file-mode
  (require-and-fetch-if-not 'toggle-file-mode :url "https://raw.github.com/gist/2600353/9a5d6965bc075c2c967b8fbb832cc4800abc14dc/toggle-file-mode.el"))
  
;; (progn ;; speedbar
;;   (require-and-fetch-if-not 'sr-speedbar)
;;   )

;; code-block
(require-and-fetch-if-not 'config-block :url "https://gist.github.com/podhmo/3917706/raw/3bd74df6fbc69995be57f4635d4b14f2afeedaa4/config-block.el")

;; python
;; (load "language.python-new")
;; (config-block-setup! 
;;  '(
;;    addon:python.core
;;    addon:python.strict.indent
;;    addon:python.find-python
;;    addon:python.import.ffap
;;    addon:python.quickrun
;;    addon:python.hook.initialize
;;    ))

(progn ;; programming-languages
  (progn ;; quick-run
    (require-and-fetch-if-not 'quickrun :url "https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el")
    (defadvice quickrun (around help-mode-after-quickrun activate)
      (lexical-let ((before-buf (current-buffer)))
        ad-do-it
        (when (and (not (equal before-buf (current-buffer)))
                   (eq major-mode 'fundamental-mode))
          (help-mode))))
    )

  (progn ;; font-lock-language-plugin
    ;; (defmacro language:define-plugin (name args &rest body)
    ;;   (declare (indent 2))
    ;;   `(defun* ,name ,args 
    ;;      (add-to-list 'language:activated-plugins ',name)
    ;;      ,@body))
    (font-lock-add-keywords
     'emacs-lisp-mode 
     '(("(\\([^\t ]*?:define-plugin\\) " (1 font-lock-keyword-face) 
        ("[^\t ]+?" nil nil (0 font-lock-function-name-face))))))
  
  (progn ;; emacs-lisp
    (require-and-fetch-if-not 'paredit)
    (define-key paredit-mode-map (kbd "C-j") ctl-j-map)

    (defun my:emacs-lisp-setup ()
      (define-many-keys emacs-lisp-mode-map
        '(("C-c C-j" . lisp-complete-symbol)
          ("C-c M-r" . paredit-forward-slurp-sexp)
          ("C-c M-R" . paredit-forward-barf-sexp)
          ("C-c M-l" . paredit-backward-slurp-sexp)
          ("C-c M-L" . paredit-backward-barf-sexp)
          ))
      (turn-on-eldoc-mode)
      (eldoc-add-command
       'paredit-backward-delete
       'paredit-close-round)
      (paredit-mode +1)
      )    
    (add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-setup))

  ;; (progn ;; yasnipet ;;move-it
  ;;   (require-and-fetch-if-not 'yasnippet)
  ;;   (yas/load-directory (concat (current-directory) "3rdparty/yasnippet-20120320/snippets")))
  
  (progn ;; auto-complete ;;move-it
    (require-and-fetch-if-not 'auto-complete)
    (setq ac-use-menu-map t)
    (define-key ac-menu-map "\C-n" 'ac-next)
    (define-key ac-menu-map "\C-p" 'ac-previous)
    )

  (progn ;; html
    (add-hook 'html-mode-hook ;; move-it
              (lambda ()
                (autopair-on)
                (modify-syntax-entry ?% "w_"))))
  (progn ;; scheme
    (load "language.scheme"))

  (progn ;; python 
    (load "language.python")

    (progn ;; activate-plugin
      (python:auto-mode-alist-plugin)
      (python:flymake-plugin)
      (python:autopair-plugin)
      (python:strict-indent-plugin)
      (python:flymake-eldoc/current-position-plugin)
      (python:anything-with-modules-plugin)
      ;; (python:yasnippet-plugin)
      ;; (python:run-program-plugin-simple)
      (python:quickrun-with-virtualenv-plugin)
      (python:auto-complete-plugin)
      (python:swap-backquote-and-underscore-plugin)
      )

    (defun my:python-setup ()
      (run-hooks 'python:plugin-mode-hook)

      (let1 keymaps
          (loop for (plugin . key-maps) in
                '((python:run-program-plugin-simple
                   . (("C-c @" . python:run-program-current-buffer)))
                  (python:anything-with-modules-plugin
                   . (("C-c C-f" . python:anything-with-modules)
                      ("C-c f" . python-describe-symbol))))
                when (python:plugin-activate-p plugin)
                append key-maps)
        (define-many-keys python-mode-map keymaps)
        (define-key python-mode-map "\C-c\C-c" 'toggle-file)
        ))

    (add-hook 'python-mode-hook 'my:python-setup))

  (progn ;; ruby
    (require-and-fetch-if-not 'flymake-ruby)
    (require 'ruby-mode nil t)
    (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  )
)

(run-hook-with-args 'after-init-hook)

(defadvice anything-M-x (before keep-history-size-more-than-one activate)
    (while (null (cdr extended-command-history))
      (push "*dummy*" extended-command-history)))

;; this is ad-hoc settings for mac
(define-key global-map "¥" (lambda (&optional n) (interactive "p") (dotimes (i (or n 1))  (insert "\\"))))
(setq-default tab-stop-list (loop for i from 4 to 120 by 4
                                  collect i))

(setq-default ring-bell-function
              (lambda () (message "ding")))

;; this is haskell settings
;; sudo port install ghc hs-cabal
;; cabal-0.14.0 update
;; cabal-0.14.0 install cabal-install
;; cabal-0.14.0 install ghc-mod hlint
(require-and-fetch-if-not 'haskell-mode)
(require-and-fetch-if-not 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) 

(defun my:haskell-cabal-home ()
  (concat (getenv "HOME") "/.cabal"))

(add-to-list 'exec-path (concat (my:haskell-cabal-home) "/bin"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(org-agenda-files (quote ("~/org/sample.org")))
 '(remember-handler-functions (quote (remember-insert-top-of-the-file))))

(defun my:haskell-setup ()
  (ghc-init)
  (flymake-mode)
  )
(add-hook 'haskell-mode-hook 'my:haskell-setup)

(add-to-list 'load-path (concat (current-directory) "ghc-mod"))
(autoload 'ghc-init "ghc" nil t)

(put 'narrow-to-region 'disabled nil)
(setq debug-on-error nil)

(defun simple-timer (n d &optional color) (interactive "nwait
ndelay")
  (run-with-timer 
   n nil 
   (lexical-let ((d d) (color (or color "#8d8d8d")))
     (lambda (&rest args)
       (lexical-let ((original-color (background-color-at-point)))
         (set-background-color color)
         (run-with-timer d nil (lambda (&rest args) (set-background-color original-color)))
         )))))

;; org-mode
(require 'org-install)
(require 'org-remember)
(add-to-list 'auto-mode-alist '("\\.notes$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("memo[0-9]+\\.txt" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun remember-insert-top-of-the-file ()
  (let ((text (buffer-string))
        (desc (remember-buffer-desc)))
    (with-temp-buffer
      (insert remember-leader-text (current-time-string)
              " (" desc ")\n\n" text)
      (let ((remember-text (buffer-string)))
        (with-current-buffer (find-file-noselect remember-data-file)
          (save-excursion
            (goto-char (point-min))
            (insert remember-text "\n")
            (goto-char (point-min))
            (when remember-save-after-remembering (save-buffer)))))
      )))
(setq remember-handler-functions '(remember-insert-top-of-the-file))

;; org-agenda
(setq calendar-holidays nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
(setq org-log-done 'time)

(setq org-agenda-custom-commands
      '(("f" occur-tree "FIXME")))


