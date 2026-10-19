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

;; Local libraries under ~/.config/emacs/lisp/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package exec-path-from-shell
  :demand t
  :config
  ;; GUI sessions on macOS lack the login shell PATH;
  ;; batch and terminal sessions already inherit it.
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package tramp
  :ensure nil
  :defer t
  :init
  (require 'tramp-cache)
  (add-to-list
   'tramp-connection-properties
   (list (rx bos "/" (or "ssh" "scp") ":")
         "login-args"
         '(("-o" "SetEnv=TERM=dumb")
           ("-l" "%u") ("-p" "%p") ("%c")
           ("-e" "none") ("%h"))))
  ;; uv tool installs (basedpyright-langserver, ruff) land in
  ;; ~/.local/bin, absent from the non-interactive SSH PATH on the
  ;; remote hosts.
  (add-to-list 'tramp-remote-path "/home/codingquark/.local/bin" t))

(use-package emacs
  :init
  ;; Remove UI clutter
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Startup configuration
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (setq initial-buffer-choice
        (lambda ()
          (require 'denote-journal)
          (denote-journal-new-or-existing-entry)))

  (defun cq-kill-initial-scratch-buffer ()
    "Kill the unused initial scratch buffer."
    (when-let* ((buffer (get-buffer "*scratch*")))
      (kill-buffer buffer)))

  (add-hook 'emacs-startup-hook #'cq-kill-initial-scratch-buffer)
  )

(defconst cq-omarchy-integration-file
  (expand-file-name "omarchy.el" user-emacs-directory)
  "Host-provided Omarchy integration shim, when available.")

(use-package server
  :ensure nil
  :init
  ;; Omarchy's local hooks expect the standard Unix socket. Keep TCP on
  ;; regular hosts for remote emacsclient access.
  (setq server-use-tcp
        (not (file-readable-p cq-omarchy-integration-file)))
  :config
  (unless (or noninteractive (server-running-p))
    (server-start)))

(use-package modus-themes
  :demand t
  :bind (("<f5>" . modus-themes-toggle))
  :config
  ;; Load through Modus so `modus-themes-after-load-theme-hook' runs; the
  ;; HEY header colours in `* Reading' are applied from that hook.
  (defun cq-macos-apply-modus-theme (appearance)
    "Load the Modus theme matching macOS APPEARANCE."
    (let ((theme (pcase appearance
                   ('light 'modus-operandi)
                   ('dark 'modus-vivendi))))
      (when (and theme (not (equal custom-enabled-themes (list theme))))
        (modus-themes-load-theme theme))))
  (unless (file-readable-p cq-omarchy-integration-file)
    (modus-themes-load-theme 'modus-vivendi)
    (when (and (eq system-type 'darwin)
               (not noninteractive)
               (boundp 'ns-system-appearance-change-functions)
               (boundp 'ns-system-appearance))
      (add-hook 'ns-system-appearance-change-functions
                #'cq-macos-apply-modus-theme)
      (cq-macos-apply-modus-theme ns-system-appearance))))

(use-package lin
  :custom
  (lin-face 'lin-blue)
  :config
  (lin-global-mode 1))

(defconst cq-variable-pitch-font "Charter"
  "Font family used by variable-pitch faces.")

(defvar cq-font-family "JetBrainsMono Nerd Font"
  "Default font family for the `default' face.
Hosts override this in `cq-private-fonts-file'.")

(defvar cq-font-height 160
  "Default height for the `default' face, in tenths of a point.
Hosts override this in `cq-private-fonts-file'.")

(defvar cq-private-fonts-file
  (expand-file-name "private/fonts.el" user-emacs-directory)
  "Host-local font overrides, absent on hosts that keep the defaults.")

(load cq-private-fonts-file 'noerror 'nomessage)

(defun cq-apply-frame-fonts (frame)
  "Apply installed fonts to graphical FRAME."
  (when (display-graphic-p frame)
    (when (find-font (font-spec :family cq-variable-pitch-font) frame)
      ;; Reading-oriented modes like elfeed-show rely on `variable-pitch`.
      (set-face-attribute 'variable-pitch frame :family cq-variable-pitch-font))
    (when (find-font (font-spec :family cq-font-family) frame)
      (set-face-attribute 'default frame
                          :family cq-font-family
                          :height cq-font-height))))

(add-hook 'after-make-frame-functions #'cq-apply-frame-fonts)
(mapc #'cq-apply-frame-fonts (frame-list))

(use-package emacs
  :init
  (setq-default mode-line-format
                '("%*"
                  (:eval (when (file-remote-p default-directory) "@"))
                  " "
                  mode-line-buffer-identification
                  "%e"
                  "%[" "%n" "%]"
                  erc-modified-channels-object
                  mode-line-format-right-align
                  (vc-mode ("  " vc-mode))
                  (flymake-mode ("  " flymake-mode-line-counters))
                  "  "
                  mode-name
                  " L%l:C%c")))

(use-package emacs
  :init
  ;; Indentation
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)

  ;; File management
  (setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
  (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

  ;; Visual aids
  (show-paren-mode 1)
  ;; macOS compatibility
  (setq dired-use-ls-dired nil)

  :hook ((dired-mode . auto-revert-mode)
         (prog-mode . auto-revert-mode)
         (text-mode . auto-revert-mode))

  ;; Better keybindings
  :bind
  ("C-x C-b" . ibuffer))

(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode 1))

(use-package hideshow
  :ensure nil
  :hook (js-json-mode . hs-minor-mode))

(use-package flyspell
  :ensure nil
  :hook (text-mode . flyspell-mode)
  :custom
  (ispell-program-name "aspell"))

(use-package dired
  :ensure nil
  :hook ((dired-mode . denote-dired-mode)
         (dired-mode . dired-hide-details-mode)))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font Mono"))

(use-package nerd-icons-dired
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode))

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

(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package vertico-prescient
  :after (vertico prescient)
  :custom
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode 1))

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

(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  ;; Explicit-only completion: popup on TAB / C-M-i, never on idle.
  (corfu-auto nil)
  (corfu-cycle t))

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

(use-package magit
  :commands magit-project-status
  :bind (("C-x g" . magit-status))
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(magit-project-status "Magit" ?m) t)))

(use-package magit-delta
  :if (executable-find "delta")
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package magit-pi
  :load-path "~/workspace/magit-pi"
  :ensure nil
  :after git-commit
  :bind (:map git-commit-mode-map
              ("M-g" . magit-pi-generate))
  :custom
  ;; (magit-pi-model "openai-codex/gpt-6-astra")
  (magit-pi-model "gluon/Qwen3.8-Flash-Next")
  (magit-pi-provider-extensions '("~/.pi/agent/extensions/lan-provider.ts"))
  (magit-pi-explore t)
  (magit-pi-debug t)
  (magit-pi-prompt "
Write only a plain-text commit message for the staged diff. Do not use
any other formatting.  Use a concrete imperative subject: aim for 50
characters, max 72; no trailing period or Conventional Commits prefix.
Add a body only for useful rationale, behavior, or tradeoffs. Separate
with a blank line; wrap at 72 characters. Explain, don't list files.

The staged diff defines scope; the draft may clarify intent.
If tools are available, consult relevant repository callers, tests,
or docs only as needed. Keep unstaged working-tree changes out of the
message.
"))

(use-package erc
  :ensure nil
  :commands (erc erc-tls cq-erc-libera cq-erc-libera-password)
  :bind (("C-c i" . cq-erc-libera))
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
  (require 'erc-sasl)
  (require 'gnutls)
  (erc-services-mode 1)
  (erc-match-mode 1)
  (erc-track-mode 1)
  (erc-autojoin-mode 1)

  (add-hook 'erc-mode-hook (lambda () (display-line-numbers-mode -1)))

  (defvar cq-erc-libera-key-uri nil
    "PKCS#11 key URI, configured in private/erc/config.el.")
  (defvar cq-erc-libera-certificate
    (expand-file-name "private/erc/libera-yubikey.pem" user-emacs-directory)
    "Public certificate enrolled with Libera NickServ.")

  (load (expand-file-name "private/erc/config.el" user-emacs-directory)
        'noerror 'nomessage)

  (defvar-local cq-erc-libera-sasl-authenticated nil)

  (defun cq-erc-libera-tls (name buffer host port &rest parameters)
    "Open Libera TLS with a YubiKey PIN entered in the minibuffer.
  Use the usual ERC connector NAME, BUFFER, HOST, PORT and PARAMETERS."
    (unless (and (equal host "irc.libera.chat") (equal port 6697))
      (error "This connector is only for irc.libera.chat:6697"))
    (let ((certificate (plist-get parameters :client-certificate))
          (debug-on-error nil) (debug-on-quit nil)
          pin tls process ready)
      (unless (and certificate (file-readable-p (cadr certificate)))
        (user-error "Missing Libera certificate: %s" (cadr certificate)))
      (unwind-protect
          (progn
            (setq pin (read-passwd "YubiKey PIV PIN for Libera: "))
            (when (string-empty-p pin) (user-error "Empty PIN; cancelled"))
            (message "Touch the YubiKey when it flashes; Emacs may pause while waiting")
            (redisplay t)
            (setq tls (gnutls-boot-parameters
                       :type 'gnutls-x509pki :hostname host :verify-error t
                       :pass pin :keylist (list certificate))
                  process (make-network-process
                           :name name :buffer buffer :host host :service port
                           :nowait t :noquery t
                           :tls-parameters (cons 'gnutls-x509pki tls)))
            (let ((deadline (+ (float-time) 45)))
              (while (and (eq (process-status process) 'connect)
                          (< (float-time) deadline))
                (accept-process-output process 0.1)))
            (unless (eq (process-status process) 'open)
              (user-error "YubiKey TLS failed; check the key, PIN and touch, then retry manually"))
            (setq ready t)
            process)
        (when pin (clear-string pin))
        (when tls (setf (plist-get tls :pass) nil))
        (when (and process (not ready)) (delete-process process)))))

  (defun cq-erc-libera-sasl-success (_process _response)
    "Record SASL success before ERC finishes registration."
    (setq cq-erc-libera-sasl-authenticated t)
    nil)

  (defun cq-erc-libera-require-sasl (process _response)
    "Reject registration without successful SASL authentication."
    (unless cq-erc-libera-sasl-authenticated
      (delete-process process)
      (message "Libera disconnected: SASL EXTERNAL did not succeed")
      t))

  (defun cq-erc-libera-session-setup ()
    "Keep YubiKey sessions manual and disable password fallback."
    (when (eq erc-session-connector #'cq-erc-libera-tls)
      (setq-local erc-server-auto-reconnect nil
                  erc-auth-source-server-function nil
                  erc-use-auth-source-for-nickserv-password nil
                  erc-prompt-for-nickserv-password nil
                  erc-nickserv-passwords nil
                  erc-autojoin-timing 'connect
                  cq-erc-libera-sasl-authenticated nil)
      (add-hook 'erc-server-903-functions #'cq-erc-libera-sasl-success nil t)
      (add-hook 'erc-server-001-functions #'cq-erc-libera-require-sasl nil t)))

  (add-hook 'erc-mode-hook #'cq-erc-libera-session-setup)

  (defun cq-erc-libera (&optional nick)
    "Connect to Libera using YubiKey-backed SASL EXTERNAL.
  Use optional NICK for a separate connection."
    (interactive)
    (require 'erc-sasl)
    (unless (and (stringp cq-erc-libera-key-uri)
                 (string-prefix-p "pkcs11:" cq-erc-libera-key-uri))
      (user-error "Set cq-erc-libera-key-uri in private/erc/config.el"))
    (let ((erc-server-connect-function #'cq-erc-libera-tls)
          (erc-auth-source-server-function nil)
          (erc-sasl-mechanism 'external)
          (erc-sasl-authzid nil)
          (erc-sasl-password nil)
          (erc-sasl-auth-source-function nil)
          (erc-modules (cons 'sasl (remq 'sasl erc-modules))))
      (erc-tls :server "irc.libera.chat" :port 6697
               :nick (or nick erc-nick) :user "codingquark"
               :full-name erc-user-full-name
               :client-certificate (list cq-erc-libera-key-uri
                                         cq-erc-libera-certificate))))

  (defun cq-erc-libera-password ()
    "Explicitly use the previous TLS/NickServ login for recovery."
    (interactive)
    (let ((erc-server-connect-function #'erc-open-tls-stream)
          (erc-modules (remq 'sasl erc-modules)))
      (erc-tls :server "irc.libera.chat" :port 6697
               :nick erc-nick :full-name erc-user-full-name))))

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

(defun cq-hey-style-modus-header-faces (&rest _)
  "Apply the active Modus palette to HEY's header-line elements.

Only the elements that must differ from plain header text get a spec:
the source name takes `accent-1', the same entry Modus gives
`elfeed-search-feed-face', so feed names read alike in both clients; the
refresh time takes the date accent; warnings and failures take Modus's
UI-area colours, which hold up better than the buffer-area ones on the
header's `bg-dim'.  Brand, separators, and ordinary state are already dim
through `shadow', while account, subject, and count stay neutral so
`header-line-inactive' can dim them in an unfocused window.  Specs
restate each `defface' inheritance, because a theme spec replaces the
default spec rather than merging into it."
  (when-let* ((theme (modus-themes-get-current-theme)))
    ;; Look the colours up one by one: `modus-themes-with-colors' splices its
    ;; body into an `eval', which warns about the palette names when compiled.
    ;; CLASS is the colour-display test that macro binds as `c'.
    (let ((class '((class color) (min-colors 256))))
      (custom-theme-set-faces
       theme
       `(hey-header-source-face
         ((,class :inherit (mode-line-buffer-id header-line)
                  :foreground ,(modus-themes-get-color-value
                                'accent-1 'with-overrides theme))))
       `(hey-header-updated-face
         ((,class :inherit (shadow header-line)
                  :foreground ,(modus-themes-get-color-value
                                'date-common 'with-overrides theme))))
       `(hey-header-warning-face
         ((,class :inherit (hey-warning-face header-line)
                  :foreground ,(modus-themes-get-color-value
                                'modeline-warning 'with-overrides theme))))
       `(hey-header-error-face
         ((,class :inherit (hey-error-face header-line)
                  :foreground ,(modus-themes-get-color-value
                                'modeline-err 'with-overrides theme))))))))

(add-hook 'modus-themes-after-load-theme-hook
          #'cq-hey-style-modus-header-faces)

(cq-hey-style-modus-header-faces)

(defconst cq-reading-list-file
  (expand-file-name
   "Documents/notes/20230206T124634--reading-list__lists_productivity.org"
   "~")
  "Denote note used as the reading-list capture target.")

(use-package org-capture
  :ensure nil
  :demand t
  :config
  (add-to-list 'org-capture-templates
               `("w" "Web page for reading list" entry
                 (file+headline ,cq-reading-list-file "Reading list")
                 "* TODO %:description\n:PROPERTIES:\n:CAPTURED: %U\n:URL: %:link\n:END:\n\n%:annotation\n\n#+begin_quote\n%i\n#+end_quote\n\n%?"
                 :empty-lines 1)))

(use-package org-protocol
  :ensure nil
  :demand t
  :custom
  (org-protocol-default-template-key "w"))

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
  :custom
  (markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package conf-mode
  :ensure nil
  :mode ("\\.cnf\\'" . conf-unix-mode))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")

(use-package cq-home-assistant
  :ensure nil
  :demand t
  :config
  (keymap-set global-map "C-c h" cq-home-assistant-prefix-map))

;; Require eglot before the :custom keyword is processed: with :ensure
;; nil use-package defers its require until after :custom, and the
;; value would then be silently skipped by custom-theme-set-variables.
(require 'eglot)

(defun cq-python-eglot-server (&optional _interactive _project)
  "Return the basedpyright command for local or TRAMP Python buffers."
  (list (if (file-remote-p default-directory)
            ;; uv installs this server here on electron and tau.  Use an
            ;; absolute path because their non-interactive SSH PATH omits it.
            "/home/codingquark/.local/bin/basedpyright-langserver"
          "basedpyright-langserver")
        "--stdio"))

(use-package eglot
  :ensure nil
  :custom
  (eglot-workspace-configuration
   '(:rust-analyzer (:check (:command "clippy"))))
  :config
  ;; Prefer basedpyright even when another Python server is installed.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . cq-python-eglot-server)))

(defun cq-eglot-disable-python-inlay-hints ()
  "Keep Eglot inlay hints off initially in Python buffers."
  (when (and (eglot-managed-p)
             (derived-mode-p 'python-base-mode))
    (eglot-inlay-hints-mode -1)))

(add-hook 'eglot-managed-mode-hook
          #'cq-eglot-disable-python-inlay-hints)

(defun cq-eglot-format-on-save ()
  "Toggle Eglot formatting before save in Rust buffers."
  (if (eglot-managed-p)
      (when (derived-mode-p 'rust-ts-mode)
        (add-hook 'before-save-hook #'eglot-format-buffer nil t))
    (remove-hook 'before-save-hook #'eglot-format-buffer t)))

(add-hook 'eglot-managed-mode-hook #'cq-eglot-format-on-save)

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . eglot-ensure)
  :config
  (setq lua-indent-level 2))

(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :hook (rust-ts-mode . eglot-ensure))

;; python-base-mode is the shared parent of python-mode and
;; python-ts-mode, so one hook covers both, locally and over TRAMP.
(use-package python
  :ensure nil
  :hook (python-base-mode . eglot-ensure))

;; Tree-sitter: let `treesit-install-language-grammar' find the Python
;; grammar, and remap python-mode only when that grammar is ready.
(add-to-list 'treesit-language-source-alist
             '(python "https://github.com/tree-sitter/tree-sitter-python"))
(when (treesit-language-available-p 'python)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(use-package flymake-ruff)

;; Ruff lint diagnostics in local Python buffers only: flymake-ruff
;; reads the local pyproject.toml, so it must not run on remote files.
(defun cq-flymake-ruff-local-python ()
  "Load flymake-ruff in local Python file buffers only."
  (when (and (buffer-file-name)
             (not (file-remote-p (buffer-file-name))))
    (flymake-ruff-load)))

(add-hook 'python-base-mode-hook #'cq-flymake-ruff-local-python)

;; Configure Ruff for explicit `M-x apheleia-format-buffer' calls.
;; Do not enable apheleia-mode: Python formatting is never automatic.
(use-package apheleia
  :commands apheleia-format-buffer
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff)
        (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

;; pytest runner. python-mode and python-ts-mode inherit this base map.
;; Pin to regular MELPA because the stable archive currently advertises
;; a python-pytest tarball that is no longer available.
(use-package python-pytest
  :pin melpa
  :after python
  :bind (:map python-base-mode-map
              ("C-c p" . python-pytest)))

(use-package projectile
  :init
  (setq projectile-dynamic-mode-line nil)
  (setq-default projectile--mode-line "")
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-project-search-path
        '("~/workspace" "~/.config" "~/Projects" "~/Work")))

;; Group ibuffer buffers by Projectile project root. Each open
;; project gets its own section (named "Projectile: <name>"), and
;; buffers without a project fall into the default group. Rebuilds
;; the groups every time an ibuffer window is created.
(use-package ibuffer-projectile
  :after projectile
  :hook (ibuffer-mode . ibuffer-projectile-set-filter-groups))

(when (file-readable-p cq-omarchy-integration-file)
  (defun cq-omarchy-apply-modus-theme ()
    "Apply the Modus theme matching Omarchy's current appearance."
    (interactive)
    (let* ((mode (when (executable-find "omarchy-theme-color")
                   (with-temp-buffer
                     (when (eq 0 (call-process
                                  "omarchy-theme-color" nil t nil
                                  "--file" (expand-file-name
                                            "colors.toml" omarchy-theme-directory)
                                  "mode"))
                       (string-trim (buffer-string))))))
           (light (if (member mode '("light" "dark"))
                      (equal mode "light")
                    (omarchy-light-theme-p)))
           (theme (if light 'modus-operandi 'modus-vivendi)))
      ;; Disable the previous scheme by hand first: Modus only drops the
      ;; themes it recognises.  `modus-themes-load-theme' then runs the
      ;; post-load hook that restyles HEY's header line.
      (unless (equal custom-enabled-themes (list theme))
        (mapc #'disable-theme custom-enabled-themes)
        (modus-themes-load-theme theme))))

  (advice-add 'omarchy-apply-theme :override #'cq-omarchy-apply-modus-theme)
  (load cq-omarchy-integration-file nil 'nomessage)
  ;; Avoid switching away from the Omarchy-managed theme independently.
  (global-unset-key (kbd "<f5>")))
