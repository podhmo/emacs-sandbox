(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))


(defun get-go-path ()
  (let ((output (shell-command-to-string "$SHELL --login -i -c 'echo $GOPATH'")))
    (car (last (split-string output)))))

(defvar my:anything-c-source-go-src-selection
  '((name . "Go src selection")
    (init
     . (lambda ()
         (let ((buf (anything-candidate-buffer " *go src*"))
               (cmd "find $GOPATH/src -type d -mindepth 3 -maxdepth 3"))
           (flet ((display-buffer (&rest args) nil))
             (shell-command cmd buf buf))
           )))
    (real-to-display
     . (lambda (c) (replace-regexp-in-string "^.+/src/" "" c)))
    (candidates-in-buffer)
    (type . file)))

(defvar my:anything-c-std-source-go-src-selection
  '((name . "Go std src selection")
    (init
     . (lambda ()
         (let ((buf (anything-candidate-buffer " *go std src*"))
               (cmd "find `go tool dist env | grep GOROOT | cut -d = -f 2 | sed 's/\"//g;'`/src -type d -mindepth 1 -maxdepth 2"))
           (flet ((display-buffer (&rest args) nil))
             (shell-command cmd buf buf))
           )))
    (real-to-display
     . (lambda (c) (replace-regexp-in-string "^.+/src/" "" c)))
    (candidates-in-buffer)
    (type . file)))

(defun my:anything-go-src-selection ()
  (interactive)
  (let ((sources '(
                   my:anything-c-std-source-go-src-selection
                   my:anything-c-source-go-src-selection
                   )))
    (anything-other-buffer sources "*anything go packages*")))


(defun my:go-import-add (arg)
  (interactive (list current-prefix-arg))
  (let ((query (my:anything-godoc--read-query)))
    (go-import-add arg query)))



;; need pickup-file -- C-x S goimports all modified files
(defun my:go-pickup-project-directory-path ()
  (pickup-file ".git"))

(defun my:go-gofmt-modified-buffers-async ()
  (interactive)
  (let* ((project-path (file-name-directory (my:go-pickup-project-directory-path)))
         (tmp-buf (generate-new-buffer " *go-modified-file*"))
         (cmd (format "git ls-files -m %s | grep -v '/vendor/' | xargs goimports -w" project-path)))
    (lexical-let ((tmp-buf tmp-buf) (cmd cmd))
      (set-process-sentinel
       (start-process-shell-command "go-gofmt-modified-buffers-async" tmp-buf cmd)
       (lambda (sig status &rest args)
         (condition-case nil
             (progn
               (message (format "%s: %s" (replace-regexp-in-string "\n$" "" status) cmd))
               (kill-buffer tmp-buf))
           ((error) (display-buffer tmp-buf))))))))


;; zipper
(unless (fboundp 'make-zipper)
  (require 'cl)

  (cl-defstruct zipper head tail)

  (defun zipper-make-empty ()
    (make-zipper :head nil :tail nil))

  (defun zipper-last-p (z)
    (null (zipper-tail z)))

  (defun zipper-first-p (z)
    (null (zipper-head z)))

  (defun zipper-forward (z)
    (cond ((zipper-last-p z) z)
          (t (make-zipper
              :head (cons (car (zipper-tail z)) (zipper-head z))
              :tail (cdr (zipper-tail z)))
             )))

  (defun zipper-backward (z)
    (cond ((zipper-first-p z) z)
          (t (make-zipper
              :head (cdr (zipper-head z))
              :tail (cons (car (zipper-head z)) (zipper-tail z)))
             )))

  (defun zipper-insert (z e)
    (make-zipper :head (cons e (zipper-head z)) :tail (zipper-tail z)))

  (defun zipper-current (z)
    (let ((h (zipper-head z)))
      (cond ((null h) (car (zipper-tail z)))
            (t (car h)))))

  ;; TODO: test
  ;; * -> 1 -> 2 -> 3
  ;; '(() '(1 2 3))
  ;; 1 -> * ->  2 -> 3
  ;; '((1) '(2 3))
  ;; 1 -> 2 -> * -> 3
  ;; '((2 1) '(3))
  ;; 1 -> 2 -> 3 -> *
  ;; '((3 2 1) '())
  ;; 1 -> 2 -> 3 -> * -> 4
  ;; '((3 2 1) '(4))
  ;; (let* ((z0 (zipper-make-empty))
  ;;        (z1 (zipper-insert z0 1))
  ;;        (z2 (zipper-insert z1 2))
  ;;        (z3 (zipper-insert z2 3))
  ;;        (z4 (zipper-forward z3)))
  ;;   (let ((result (list z0 z1 z2 z3 z4)))
  ;;     (dolist (z result)
  ;;       (princ (zipper-current z)))))
  )

(defvar my:godoc-history (zipper-make-empty))

(defun my:go-strip-vendor-path (path)
  (replace-regexp-in-string ".+/vendor/" "" path)
  )

(defun my:godoc--get-buffer (query)
  (unless (string-equal (zipper-current my:godoc-history) query)
    (setq my:godoc-history (zipper-insert my:godoc-history query)))
  (%my:godoc--get-buffer query))

(defun %my:godoc-delete-godoc-window ()
  ;; warning
  (when (and
         (string-equal (buffer-name (current-buffer)) "*godoc*")
         (< 1 (length (window-list))))
    (delete-window))
  )

(defun my:godoc-forward ()
  (interactive)
  (%my:godoc-delete-godoc-window)
  (setq my:godoc-history (zipper-forward my:godoc-history))
  (godoc (zipper-current my:godoc-history)))

(defun my:godoc-backward ()
  (interactive)
  (%my:godoc-delete-godoc-window)
  (setq my:godoc-history (zipper-backward my:godoc-history))
  (godoc (zipper-current my:godoc-history)))


(defun my:anything-godoc--read-query ()
  (let1 r (anything-comp-read "godoc; "
                               (go--old-completion-list-style (go-packages))
                               :history go-godoc-history)
    (push r go-godoc-history)
    (my:go-strip-vendor-path r)))

(defun my:godoc (&optional query)
  (interactive)
  (let ((query (or query (my:anything-godoc--read-query))))
    (go--godoc query godoc-command)))

(with-eval-after-load 'go-mode
  (require 'insert-pair-element nil t)

  (setq my:golang-key-pair
        '(("(" . ")")
          ("\"" . "\"")
          ("'" . "'")
          ("{"  "}" "{")
          ("[" "]" "["))
        )

  (eval-after-load 'flycheck
    '(progn
       ;; (flycheck-checker-get 'go-gofmt 'next-checkers)
       (setf (get 'go-gofmt (flycheck--checker-property-name 'next-checkers))
             '((warning . go-golint)
               ;; Fall back, if go-golint doesn't exist
               (warning . go-vet)
               ;; Fall back, if go-vet doesn't exist
               (warning . go-build) (warning . go-test)
               ; (warning . go-errcheck)
               ; (warning . go-unconvert)
               )

             )
       ))



  (eval-after-load "go-mode"
    '(progn

       ;; gopath
       (let ((output (shell-command-to-string "$SHELL --login -i -c 'echo $GOPATH'")))
         (setenv "GOPATH" (car (last (split-string output)))))

       (cond ((executable-find "goimports")
              (setq gofmt-command "goimports"))
             (t (message "gorimports > gofmt!!")))

       (progn ; godoc
         (require-and-fetch-if-not 'go-eldoc)
         (add-hook 'go-mode-hook 'go-eldoc-setup)
         (setq godoc-command (if (executable-find "godoc") "godoc" "go doc"))
         (setq godoc-use-completing-read t)

         ;; popwin
         (when (boundp 'popwin:special-display-config)
           (push '("^\\*godoc [^ ]+\\*$" :regexp t) popwin:special-display-config)
           (push '("*godoc*" :dedicated t) popwin:special-display-config)
           )
         )
       (defalias 'godoc 'my:godoc)
       (defalias 'godoc--get-buffer 'my:godoc--get-buffer)

       (define-insert-pair-binding go-mode-map my:golang-key-pair)

       (require-and-fetch-if-not 'company-go) ;; require gocode

       (defun my:go-mode-setup ()
         ;; (add-hook 'before-save-hook' 'gofmt-before-save)
         ;; key bindings
         (define-key go-mode-map (kbd "C-x C-s") 'gofmt)
         (define-key go-mode-map (kbd "C-x S") 'my:go-gofmt-modified-buffers-async)
         (define-key go-mode-map (kbd "C-c C-e") 'my:godoc)
         (define-key go-mode-map (kbd "C-c C-a") 'my:go-import-add)
         (define-key go-mode-map (kbd "C-c :") 'my:anything-go-src-selection)
         (define-key go-mode-map (kbd "M-.") 'godef-jump)
         (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
         (set (make-local-variable 'company-backends) '(company-go))
         (company-mode)
         (setq indent-tabs-mode nil)
         (setq c-basic-offset 4)
         (setq tab-width 4))
       (setq go-packages-function 'go-packages-cache)
       (defun my:godoc-mode-setup ()
         (define-key godoc-mode-map (kbd "[") 'my:godoc-backward)
         (define-key godoc-mode-map (kbd "]") 'my:godoc-forward)
         (define-key godoc-mode-map (kbd "C-c C-e") 'my:godoc)
         )

       (add-hook 'go-mode-hook 'company-mode)
       (add-hook 'go-mode-hook 'flycheck-mode)
       (add-hook 'go-mode-hook 'my:go-mode-setup)
       (add-hook 'godoc-mode-hook 'my:godoc-mode-setup)
       ))

                                        ;export GOPATH=~/vboxshare/sandbox/go
  ;; (add-to-list 'exec-path (expand-file-name "/opt/local/lib/go/bin"))
  ;; (add-to-list 'exec-path (expand-file-name "~/vboxshare/sandbox/go/bin"))

  (defun go-packages-cache--clear ()
    (interactive)
    (setq go-packages-cache nil))
  (defvar go-packages-cache nil)
  (defun go-packages-cache ()
    (unless go-packages-cache
      (setq go-packages-cache
            (loop for x in (go-packages-native)
                  unless (string-match-p "/vendor/" x)
                  collect x))
      )
    go-packages-cache)
  )
