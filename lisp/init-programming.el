(require 'auto-highlight-symbol)

(add-to-list 'auto-mode-alist '("client/scripts/.+\\.js\\'" . angular-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("resources/views/.+\\.php\\'" . web-mode))

(provide 'init-programming)
