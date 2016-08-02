(require 'php-mode)
;; (require 'phpcbf)
(require 'flymake-php)

;; (custom-set-variables
;;  '(phpcbf-executable "/usr/bin/phpcbf")
;;  '(phpcbf-standard "PSR2"))

;; Auto format on save.
;; (add-hook 'php-mode-hook 'phpcbf-enable-on-save)
(add-hook 'php-mode-hook (lambda () (subword-mode 1)
                           (smartparens-strict-mode t)))

(add-hook 'php-mode-hook
          '(lambda ()
             (auto-complete-mode t)
             (require 'ac-php)
             (setq ac-sources  '(ac-source-php ) )
             (yas-global-mode 1)
             (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
             ; (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
             ))

(add-hook 'php-mode-hook 'flymake-php-load)

(provide 'init-php)
