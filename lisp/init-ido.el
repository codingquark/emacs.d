;; We want ido-mode everywhere!
;; First, enable ido itself. Then go for ido-ubiquitous
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-completing-read+)
;; (ido-completing-read+ 1) -> THIS IS WRONG

(provide 'init-ido)
