(require 'projectile)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(setq projectile-indexing-method 'alien)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(provide 'init-projectile)
