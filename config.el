(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-archive-priorities
      '(("gnu" . 30)
        ("nongnu" . 20)
        ("melpa-stable" . 10)
        ("melpa" . 0)))
(setq package-install-upgrade-built-in t)
(package-initialize)

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

(defconst cq-variable-pitch-font "Charter"
  "Font family used by variable-pitch faces.")

(when (find-font (font-spec :family cq-variable-pitch-font))
  ;; Reading-oriented modes like elfeed-show rely on `variable-pitch`.
  (set-face-attribute 'variable-pitch nil :family cq-variable-pitch-font))

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
  (elfeed-search-filter "@2-months-ago +unread")
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

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :config
  (setq gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key gptel-api-key
          :models '(openai/gpt-4.1-mini
                    nvidia/nemotron-3-super-120b-a12b:free)))
  (setq gptel-model 'openai/gpt-4.1-mini))

(use-package gptel-magit
  :after (gptel magit)
  :hook (magit-mode . gptel-magit-install)
  :config
  (defun cq-gptel-magit--forward-if-string (cb response info)
    (when (stringp response)
      (funcall cb response info)))

  (defun cq-gptel-magit--request-advice (orig-fn prompt &rest args)
    (let* ((cb (plist-get args :callback))
           (wrapped (apply-partially #'cq-gptel-magit--forward-if-string cb))
           (args (plist-put args :stream nil))
           (args (plist-put args :callback wrapped)))
      (apply orig-fn prompt args)))

  (advice-add 'gptel-magit--request :around #'cq-gptel-magit--request-advice))

(use-package erc
  :ensure nil
  :commands (erc erc-tls cq-erc-libera)
  :custom
  (erc-nick "codingquark")
  (erc-user-full-name "codingquark")
  (erc-server "irc.libera.chat")
  (erc-port 6697)
  (erc-prompt-for-password nil)
  (erc-use-auth-source-for-nickserv-password t)
  (erc-autojoin-timing 'ident)
  (erc-autojoin-channels-alist '(("Libera.Chat" "#emacs")))
  (erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"
                             "324" "329" "332" "333" "353" "477"))
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 18)
  :config
  (require 'erc-services)
  (require 'erc-match)
  (erc-services-mode 1)
  (erc-match-mode 1)
  (erc-track-mode 1)
  (erc-autojoin-mode 1)

  (add-hook 'erc-mode-hook (lambda () (display-line-numbers-mode -1)))

  (defun cq-erc-libera ()
    "Connect to Libera.Chat over TLS, reading creds from auth-source."
    (interactive)
    (erc-tls :server "irc.libera.chat" :port 6697
             :nick erc-nick :full-name erc-user-full-name)))

(defvar cq-terminal-notifier
    (executable-find "terminal-notifier")
    "Path to terminal-notifier, or nil if unavailable.")

  (defun cq-erc-notify (title message)
    "Post a macOS notification with TITLE and MESSAGE via terminal-notifier."
    (when cq-terminal-notifier
      (call-process cq-terminal-notifier nil 0 nil
                    "-title" (format "ERC: %s" title)
                    "-message" message
                    "-group" "emacs-erc"
                    "-sender" "org.gnu.Emacs")))

  (defun cq-erc-notify-match (match-type nickuserhost message)
    "Notify on `erc-match' hits for current-nick and keywords."
    (when (and (memq match-type '(current-nick keyword))
               (not (get-buffer-window (current-buffer) 'visible)))
      (let ((nick (car (split-string (or nickuserhost "") "!"))))
        (cq-erc-notify (format "%s in %s" nick (buffer-name))
                       message))))

  (defun cq-erc-notify-query (proc parsed)
    "Notify when a private message lands in a query buffer.
Returns nil so ERC keeps processing the message normally."
    (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
           (target (car (erc-response.command-args parsed)))
           (msg (erc-response.contents parsed)))
      (when (and target
                 (erc-current-nick-p target)
                 (not (erc-current-nick-p nick)))
        (let ((buf (erc-get-buffer nick proc)))
          (unless (and buf (get-buffer-window buf 'visible))
            (cq-erc-notify (format "PM from %s" nick) msg)))))
    nil)

  (with-eval-after-load 'erc-match
    (add-hook 'erc-text-matched-hook #'cq-erc-notify-match))

  (with-eval-after-load 'erc
    (add-hook 'erc-server-PRIVMSG-functions #'cq-erc-notify-query))

(use-package denote
  :hook (text-mode . denote-fontify-links-mode-maybe)
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
  :config
  (setq olivetti-body-width 80))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
