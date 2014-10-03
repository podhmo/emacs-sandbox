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
     (require-and-fetch-if-not 'go-eldoc)
     (add-hook 'go-mode-hook 'go-eldoc-setup)
     (define-insert-pair-binding go-mode-map my:golang-key-pair)
     ;; key bindings
     (define-key go-mode-map (kbd "C-x C-s") 'gofmt)
     (define-key go-mode-map (kbd "C-c C-e") 'godoc)
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)

     (require 'flymake-go)
     ))

;export GOPATH=~/vboxshare/sandbox/go
(add-to-list 'exec-path (expand-file-name "/usr/local/go/bin"))
(add-to-list 'exec-path (expand-file-name "~/vboxshare/sandbox/go/bin"))

(defvar go-packages-cache nil)
(defun go-packages-cache ()
  (unless go-packages-cache
    (setq go-packages-cache (go-packages))
    )
  go-packages-cache)

(when (file-exists-p "~/vboxshare/sandbox/go/src/github.com/dougm/goflymake")
  (add-to-list 'load-path "~/vboxshare/sandbox/go/src/github.com/dougm/goflymake")
  (require 'go-flymake)
  )
