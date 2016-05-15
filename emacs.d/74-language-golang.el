(require 'go-mode)
(require 'insert-pair-element nil t)

(setq my:golang-key-pair
      '(("(" . ")")
        ("\"" . "\"")
        ("'" . "'")
        ("{"  "}" "{")
        ("[" "]" "["))
      )

(eval-after-load "go-mode"
  '(progn
     (setenv "GOPATH" (format "%s/vboxshare/sandbox/go" (getenv "HOME")))

     (progn ; godoc
       (require-and-fetch-if-not 'go-eldoc)
       (add-hook 'go-mode-hook 'go-eldoc-setup)

       (setq godoc-use-completing-read t)
       (when (boundp 'popwin:special-display-config)
         (push '("^\\*godoc [^ ]+\\*$" :regexp t) popwin:special-display-config)
         )
       )

     (define-insert-pair-binding go-mode-map my:golang-key-pair)

     (require-and-fetch-if-not 'company-go) ;; require gocode
     (add-hook 'go-mode-hook 'company-mode)
     ;; (add-hook 'go-mode-hook 'flycheck-mode)

     (add-hook 'go-mode-hook
               (lambda()
                 (add-hook 'before-save-hook' 'gofmt-before-save)
                 ;; key bindings
                 (define-key go-mode-map (kbd "C-x C-s") 'gofmt)
                 (define-key go-mode-map (kbd "C-c C-e") 'godoc)
                 (define-key go-mode-map (kbd "M-.") 'godef-jump)
                 (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
                 (set (make-local-variable 'company-backends) '(company-go))
                 (company-mode)
                 (setq indent-tabs-mode nil)
                 (setq c-basic-offset 4)
                 (setq tab-width 4)))
     ))

;export GOPATH=~/vboxshare/sandbox/go
(add-to-list 'exec-path (expand-file-name "/opt/local/go/bin"))
(add-to-list 'exec-path (expand-file-name "~/vboxshare/sandbox/go/bin"))

(defvar go-packages-cache nil)
(defun go-packages-cache ()
  (unless go-packages-cache
    (setq go-packages-cache (go-packages))
    )
  go-packages-cache)
