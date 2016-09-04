;; (require 'haxe-mode)
(autoload 'haxe-mode "haxe-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.hx$" . haxe-mode))

(with-eval-after-load "haxe-mode"
  (defconst my-haxe-style
    '("java" (c-offsets-alist . ((case-label . +)
                                 (arglist-intro . +)
                                 (arglist-cont-nonempty . 0)
                                 (arglist-close . 0)
                                 (cpp-macro . 0))))
    "My haXe Programming Style")

  (add-hook 'haxe-mode-hook
            (function
             (lambda ()
               (c-add-style "haxe" my-haxe-style t)
               (setq tab-width 4)
               (setq indent-tabs-mode nil)
               (local-set-key [(return)] 'newline-and-indent))))

  (when (require 'insert-pair-element nil t)  
    (setq haxe-selfish:mapping
          `(("\\" . insert-pair-escaped-after)
            ("," . ,(ilambda (insert ", ")))))
    
    (setq haxe-selfish:key-pair
          '(("(" . ")")
            ("\"" . "\"")
            ("'" . "'")
            ("{"  "}" "{")
            ("[" "]" "[")))

    (defun haxe-selfish:install ()
      (loop for (k . f) in haxe-selfish:mapping
            do (define-key haxe-mode-map k f))
      (define-insert-pair-binding haxe-mode-map haxe-selfish:key-pair)
      )
    (add-hook 'haxe-mode-hook 'haxe-selfish:install)))
