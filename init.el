;; Load literate configuration
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cq-home-assistant-entity-id "sensor.weatherstation_ground_bme280_bme280_temperature")
 '(package-selected-packages
   '(denote-journal denote-menu elfeed embark-consult gptel-magit helpful
                    lin marginalia markdown-mode modus-themes olivetti
                    orderless prescient vertico vertico-prescient)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
