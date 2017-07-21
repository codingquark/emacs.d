(add-hook 'prog-mode-hook
          (global-hl-line-mode)
          (global-diff-hl-mode))

(put 'narrow-to-region 'disabled nil)

(provide 'init-prog)
