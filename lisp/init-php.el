(require 'php-mode)
;;(require 'phpcbf)
(require 'flymake-php)

;; (custom-set-variables
;;  '(phpcbf-executable "/usr/bin/phpcbf")
;;  '(phpcbf-standard "PSR2"))

;; Auto format on save.
;;(add-hook 'php-mode-hook 'phpcbf-enable-on-save)
(add-hook 'php-mode-hook (lambda () (subword-mode 1)
                           (smartparens-strict-mode t)))

(add-hook 'php-mode-hook 'flymake-php-load)

(provide 'init-php)
