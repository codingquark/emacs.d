;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ;; Org-mode's repository
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; (require 'package)
;; (package-initialize)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ;; Org-mode's repository

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
(require 'init-ido)
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
(require 'init-mutt)
(require 'init-stumpwm)
(require 'init-mu4e)
(require 'init-nov)
(require 'custom)

(setq pomidor-sound-tick nil
      pomidor-sound-tack nil
      pomidor-sound-overwork nil)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(server-start)

;;; init.el ends here
