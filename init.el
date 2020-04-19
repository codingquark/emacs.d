;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'package)

;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ;; Org-mode's repository

(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; (setq sentence-end-base "[.?!।][]\"'”)}]*") ;; Try this if you happen to open Hindi
(require 'init-install)
(require 'better-defaults)
(require 'init-erc)
(require 'init-smex)
;; (require 'init-ido)
(require 'init-ivy)
(require 'init-smartparens)
(require 'init-python)
;; (require 'init-music)
;; Trying mu4e
;; (require 'init-gnus)
;; (require 'gnus-notify+)
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
(require 'init-prog)
(require 'init-resize-window)
(require 'init-alarm)
(require 'init-org)
(require 'init-switchwindow)
;; (require 'init-arch)
(require 'init-elfeed)
(require 'init-browser)
;; (require 'init-mutt)
(require 'init-stumpwm)
(require 'init-mu4e)
(require 'init-nov)
(require 'init-mastodon)
;; (require 'init-pomodoro)
;; (require 'init-todoist)
(require 'init-theme)
(require 'custom)
(require 'init-exwm)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (setq package-check-signature nil)

(exwm-enable)

(server-start)

;;; init.el ends here
(put 'list-timers 'disabled nil)
