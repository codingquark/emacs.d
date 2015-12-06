;; We want ido-mode everywhere!
;; First, enable ido itself. Then go for ido-ubiquitous
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(provide 'init-ido)
