;;(require 'auto-highlight-symbol)

(add-to-list 'auto-mode-alist '("client/scripts/.+\\.js\\'" . angular-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("resources/views/.+\\.php\\'" . web-mode))
(global-set-key (kbd "C-c g") 'rgrep)
(setq-default line-spacing 5) ;; Checkout `line-height` instead
(diff-hl-mode 1)
(diff-hl-dired-mode 1)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(add-to-list 'default-frame-alist '(font . "Source Code Pro-10" ))
(set-face-attribute 'default t :font "Source Code Pro-10")

(provide 'init-programming)
