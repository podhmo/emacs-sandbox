(require-and-fetch-if-not 'flymake-ruby)
(require 'ruby-mode nil t)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
)
