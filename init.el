;; Load literate configuration
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cq-home-assistant-entity-id "sensor.weatherstation_ground_bme280_bme280_temperature")
 '(custom-safe-themes
   '("967c23e9ba179b80560774419f081df22e7674aac23c5c550b817e4a1ce7d058"
     "7e98dc1aa7f5db0557691da690c38d55e83ddd33c6d268205d66e430d57fb982"
     "6dcf1ca4c7432773084b9d52649ee5eb2c663131c4c06859f648dea98d9acb3e"
     default))
 '(package-selected-packages
   '(apheleia catppuccin-theme dash denote-journal denote-menu
              elfeed embark-consult flymake-ruff gptel-magit
              gptel-prompts helpful lin marginalia markdown-mode
              modus-themes olivetti orderless prescient project
              python-pytest s transient vertico vertico-prescient)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
