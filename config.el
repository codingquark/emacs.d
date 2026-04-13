(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package emacs
  :init
  ;; Remove UI clutter
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  
  ;; Startup configuration
  (setq inhibit-startup-message t)
  (setq initial-major-mode 'org-mode)
  (setq initial-scratch-message "")
  
  ;; Start in denote directory
  (setq initial-buffer-choice (lambda () (dired "~/Documents/notes"))))

(use-package modus-themes
  :init
  (load-theme 'modus-operandi t))

(when (string= system-name "muon.local")
  (set-face-attribute 'default nil :font "IBM Plex Mono" :height 160))

(use-package emacs
  :init
  ;; Indentation
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  
  ;; File management
  (setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
  (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))
  
  ;; Visual aids
  (global-display-line-numbers-mode 1)
  (show-paren-mode 1)
  (global-auto-revert-mode 1)
  
  ;; macOS compatibility
  (setq dired-use-ls-dired nil)
  
  ;; Better keybindings
  :bind
  ("C-x C-b" . ibuffer))

(use-package dired
  :ensure nil
  :hook ((dired-mode . denote-dired-mode)
         (dired-mode . dired-hide-details-mode)))

(use-package denote
  :hook (find-file . denote-fontify-links-mode)
  :bind (
    ("C-c n n" . denote)
    ("C-c n D" . cq-open-denote-directory)
    ("C-c n N" . denote-type)
    ("C-c n i" . denote-link)
    ("C-c n I" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n f b" . denote-find-backlink)
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)
    ;; ("C-c n ." . cq-insert-time-stamp)
    )
  :custom
  (denote-directory "~/Documents/notes")
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type 'text)
  (denote-prompts '(title keywords))
  :config
  (setq crm-separator ",")
  (defun cq-open-denote-directory ()
    (interactive)
    (revert-buffer (dired denote-directory)))
  (defun cq-insert-time-stamp ()
    "Insert a timestamp with a newline character."
    (interactive)
    (insert (current-time-string))
    (newline)))

(use-package denote-journal
  :after denote
  :bind
  (("C-c n j" . denote-journal-new-or-existing-entry))
  :custom
  (denote-journal-title-format 'day-date-month-year))

(use-package denote-menu
  :after denote)

(use-package olivetti
  :hook ((text-mode . olivetti-mode)
         (org-mode . olivetti-mode))
  :config
  (setq olivetti-body-width 80))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
