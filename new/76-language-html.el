(add-hook 'html-mode-hook ;; move-it
            (lambda ()
              (modify-syntax-entry ?% "w_")))
