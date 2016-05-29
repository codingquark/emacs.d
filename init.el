(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ;; Org-mode's repository
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-install)
(require 'better-defaults)
(require 'init-erc)
(require 'init-smex)
(require 'init-ido)
(require 'init-smartparens)
(require 'init-python)
(require 'init-music)
(require 'init-gnus)
(require 'init-autocomplete)
(require 'init-flycheck)
(require 'init-magit)
(require 'init-yas)
(require 'init-php)
(require 'init-dired)
(require 'init-programming)
(require 'init-cursors)
(require 'init-projectile)
(require 'init-grep)
(require 'custom)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;; init-local.el ends here

(put 'narrow-to-region 'disabled nil)
