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
  ;; (setq initial-buffer-choice (lambda () (dired "~/Documents/notes")))
  )

(use-package modus-themes
  :init
  (load-theme 'modus-operandi t))

(when (or (string= system-name "muon.local") (string= system-name "photon"))
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
  ;; macOS compatibility
  (setq dired-use-ls-dired nil)

  :hook ((dired-mode . auto-revert-mode)
         (prog-mode . auto-revert-mode)
         (text-mode . auto-revert-mode))

  ;; Better keybindings
  :bind
  ("C-x C-b" . ibuffer))

(use-package vertico
  :init
  (vertico-mode 1)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.5))

(use-package helpful
  :bind (([remap describe-command] . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200))

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1))

(use-package winner
  :ensure nil
  :init
  (winner-mode 1))

(defvar cq-elfeed-feeds-file
  (expand-file-name "elfeed-feeds.el" user-emacs-directory)
  "Path to the personal Elfeed feed list.")

(use-package elfeed
  :bind (("C-c f" . elfeed)
         ("C-c F" . cq-open-elfeed-feeds-file))
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-search-filter "@6-months-ago +unread")
  :config
  ;; Keep subscriptions outside the main config to make feed edits low-friction.
  (load cq-elfeed-feeds-file 'noerror 'nomessage)

  (defun cq-open-elfeed-feeds-file ()
    "Open the Elfeed subscriptions file."
    (interactive)
    (find-file cq-elfeed-feeds-file)))

(use-package dired
  :ensure nil
  :hook ((dired-mode . denote-dired-mode)
         (dired-mode . dired-hide-details-mode)))

(use-package denote
  :hook (text-mode . denote-fontify-links-mode)
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
  :hook ((org-mode . olivetti-mode))
  :config
  (setq olivetti-body-width 80))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
