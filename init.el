;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ;; Org-mode's repository

(package-initialize)

;; TODO define list of packages and install them

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(eval-when-compile
  (require 'use-package))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; (require 'init-install)
(use-package better-defaults)

(use-package erc
  :bind ("C-c i e" . (lambda () (interactive) (switch-to-buffer "#emacs")))
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-paranoid t)
  (erc-autojoin-mode 1)
  ;; (setq erc-pals
  ;;       '("twb" "thebigj" "technomancy" "forcer" "wasamasa" "parjanya" "JordiGH" "parsnip" "oshor" "mbuf" "aidalgol" "bremner"))
  (setq erc-keywords '("codingquark" "codingquark_"))
  (setq erc-max-buffer-size 20000)
  (setq erc-truncate-buffer-on-save t)
  (erc-completion-mode t)
  (erc-match-mode t)
  (erc-hl-nicks-enable)
  (setq erc-modules
        '(autojoin
          button
          completion
          fill
          irccontrols
          keep-place
          list
          log
          match
          menu
          move-to-prompt
          netsplit
          networks
          noncommands
          notifications
          readonly
          ring
          stamp
          spelling
          track)))

(use-package ivy
  :bind (("C-s" . 'swiper)
         ("M-x" . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file)
         ("C-x b" . 'ivy-switch-buffer))
  :config
  (setq ivy-use-virtual-buffers t)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (ivy-mode 1))

(use-package smartparens
  :config
  (smartparens-global-mode t))

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-exec-path '("/usr/bin/python3"))
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3"))

(use-package flycheck
  :hook (after-init-hook . global-flycheck-mode))

(use-package magit
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (unpushed . hide))))

(use-package php-mode
  :hook (php-mode-hook . flymake-php-load)
  :config
  (subword-mode t)
  (smartparens-strict-mode t)
  (auto-complete-mode t))

(use-package grep
  :bind ("C-c g" . rgrep)
  :hook (grep-mode-hook . hl-line-mode))

(use-package diff-hl
  :init
  (diff-hl-mode t)
  (diff-hl-dired-mode t))

(use-package hl-line
  :hook ((prog-mode . global-hl-line-mode)
         (prog-mode . global-diff-hl-mode))
  :config
  (global-hl-line-mode))

(use-package autorevert
  :init
  (global-auto-revert-mode t))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (projectile-mode +1))

(use-package resize-window
  :bind ("C-c ;" . resize-window))

(use-package org-mode
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c C-q" . counsel-org-tag))
  :hook ((org-mode . turn-on-flyspell)
         (org-mode . auto-fill-mode))
  :config
  (setq org-directory "~/Documents/org")
  (setq org-brain-path (concat org-directory "/brain")) ;; No need to do that as it defaults to org-directory/brain
  (setq org-id-locations-file (concat user-emacs-directory "/.org-id-locations"))
  (setq org-archive-location "~/Documents/org/archive.org::* From %s")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (list (concat org-directory "/todo.org")))
  (setq org-deadline-warning-days 14)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-reverse-note-order t)
  (setq org-tags-column -80)
  (setq org-agenda-restore-windows-after-quit 1)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "DEFERRED(f)"
           "DELEGATED(l)"
           "STARTED(s)"
           "WAITING(w)"
           "|"
           "DONE(d)"
           "CANCELLED(x)")))
  (setq org-agenda-custom-commands
        (quote (("d" todo "DELEGATED" nil)
                ("c" todo "DONE|DEFERRED|CANCELLED" nil)
                ("w" todo "WAITING" nil)
                ("W" agenda "" ((org-agenda-ndays 21)))
                ("A" agenda ""
                 ((org-agenda-skip-function
                   (lambda nil
                     (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
                  (org-agenda-ndays 1)
                  (org-agenda-overriding-header "Today's Priority #A tasks: ")))
                ("u" alltodo ""
                 ((org-agenda-skip-function
                   (lambda nil
                     (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                               (quote regexp) "\n]+>")))
                  (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
  (setq org-capture-templates
        '(("t" "Todo" entry (file (lambda () (concat org-directory "/todo.org")))
           "* TODO %? %^G\n %i\n %a %u")
          ("n" "Note" entry (file+headline (lambda () (concat org-directory "/notes.org")) "Notes")
           "* %?\n %i\n %u %^G")
          ("s" "Schedule toots" plain (file "~/Documents/org/scheduled_toots.org")
           "%?%i")
          ("b" "Brain" plain (function org-brain-goto-end)
           "* %i%?" :empty-lines 1)))
  ;; Large LaTeX previews
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
  ;; Do not export with sub-super-scripts
  (setq org-export-with-sub-superscripts nil))

(use-package org-brain
  :bind ("C-c b" . org-brain-visualize)
  :config
  ;; (setq org-brain-path (concat org-directory "/brain")) ;; No need to do that as it defaults to org-directory/brain
  (setq org-id-track-globally t)  ;; this is t by default, merely making sure
  (setq org-id-locations-file (concat user-emacs-directory "/.org-id-locations"))
  (setq org-brain-show-resources t))  ;; Clean org-brain-visualize

(use-package switch-window
  :bind ("C-x o" . switch-window)
  :config
  (setq switch-window-shortcut-style 'alphabet))

(use-package eww
  :config
  (setq browse-url-browser-function '(("\\(wikipedia.org\\)\\|\\(sprunge.us\\)" . eww-browse-url)
                                    ("\\(meet.google.com\\)" . browse-url-chrome)
                                    (".*" . browse-url-firefox)))
  (setq eww-search-prefix "https://duckduckgo.com/html/?q="))

(use-package w3m
  :config
  (setq w3m-search-default-engine "duckduckgo"))

(use-package stumpwm-mode
  :config
  (setq stumpwm-shell-program "/home/codingquark/workspace/stumpwm-contrib/util/stumpish/stumpish"))

(use-package mu4e
  :bind ("C-c m" . mu4e)
  :config
  ;; Let's fetch the mails!
  ;; No need to run `mu4e-update-index` periodically.
  ;; (setq mu4e-get-mail-command "/usr/bin/offlineimap -o -u syslog"
  ;;       mu4e-update-interval (* 3 60)
  ;;       mu4e-index-cleanup t n)
  (setq mu4e-get-mail-command "/usr/bin/mbsync -a -q"
        mu4e-update-interval (* 30 60)  ;; Every 30min
        mu4e-index-cleanup t)
  ;; Suppress the minibuf messages for updates, kind of annoying
  (defun suppress-messages (old-fun &rest args)
    (cl-flet ((silence (&rest args) (ignore)))
      (advice-add 'message :around #'silence)
      (unwind-protect
          (apply old-fun args)
        (advice-remove 'message #'silence))))

  (with-eval-after-load "mu4e"
    (advice-add 'mu4e-update-mail-and-index :around #'suppress-messages)
    (advice-add 'mu4e-index-message :around #'suppress-messages)
    (advice-add 'progress-reporter-done :around #'suppress-messages))

  (setq mu4e-hide-index-messages t)

  ;; Get alerts after receiving the mails
  (mu4e-alert-set-default-style 'notifications)
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)
  (setq mu4e-maildir "~/Mail"
        mu4e-sent-folder "/protonmail/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder "/protonmail/Trash"
        mu4e-refile-folder "/protonmail/Archive")
  (setq mu4e-user-mail-address-list '("dhavan@dwayo.com" "quark@codingquark.com" "codingquark@tilde.team" "dhavan@livbim.io"))
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Private"
             :enter-func (lambda () (mu4e-message "Entering Private context"))
             :leave-func (lambda () (mu4e-message "Leaving Private context"))
             ;; Wee match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "quark@codingquark.com")))
             :vars '((user-mail-address      . "quark@codingquark.com")
                     (smtpmail-smtp-user     . "quark@codingquark.com")
                     (user-full-name         . "Dhavan Vaidya")
                     (mu4e-compose-signature .
                                             (concat
                                              "D Vaidya"))
                     ;; (mu4e-maildir           . "~/Mail/protonmail")
                     (mu4e-sent-folder       . "/Sent")
                     (mu4e-drafts-folder     . "/Drafts")
                     (mu4e-trash-folder      . "/Trash")
                     (mu4e-refile-folder     . "/protonmail/Archive")))
           ,(make-mu4e-context
             :name "LivBIM"
             :enter-func (lambda () (mu4e-message "Entering LivBIM context"))
             :leave-func (lambda () (mu4e-message "Leaving LivBIM context"))
             ;; We match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (string= (mu4e-message-field msg :maildir) "/livbimgmail")
                             ;; (mu4e-message-contact-field-matches msg :to "dhavan@livbim.io")
                             ))
             :vars '((user-mail-address      . "dhavan@livbim.io")
                     (smtpmail-smtp-user     . "dhavan@livbim.io")
                     (user-full-name         . "Dhavan")
                     (mu4e-compose-signature .
                                             (concat
                                              "---\n"
                                              "- Dhavan"))
                     ;; (mu4e-maildir           . "~/Mail/livbimgmail")
                     (mu4e-drafts-folder     . "/[Gmail].Drafts")
                     (mu4e-trash-folder      . "/[Gmail].Trash")
                     (mu4e-sent-folder     . "/[Gmail].Sent Mail")))
           ,(make-mu4e-context
             :name "Dwayo"
             :enter-func (lambda () (mu4e-message "Entering Dwayo context"))
             :leave-func (lambda () (mu4e-message "Leaving Dwayo context"))
             ;; We match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg :to "dhavan@dwayo.com")))
             :vars '((user-mail-address      . "dhavan@dwayo.com")
                     (smtpmail-smtp-user     . "dhavan@dwayo.com")
                     (user-full-name         . "Dhavan")
                     (mu4e-compose-signature .
                                             (concat
                                              "---\n"
                                              "- Dhavan V"))
                     (mu4e-drafts-folder     . "/Drafts")
                     (mu4e-trash-folder      . "/Trash")
                     (mu4e-sent-folder       . "/Sent")
                     (mu4e-refile-folder     . "/protonmail/Archive")))
           ,(make-mu4e-context
             :name "Tilde"
             :enter-func (lambda () (mu4e-message "Entering Tilde context"))
             :leave-func (lambda () (mu4e-message "Leaving Tilde context"))
             ;; Wee match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "codingquark@tilde.team")))
             :vars '((user-mail-address      . "codingquark@tilde.team")
                     (smtpmail-smtp-user     . "codingquark@tilde.team")
                     (user-full-name         . "~codingquark")
                     (mu4e-compose-signature .
                                             (concat
                                              "~codingquark"))
                     ;; (mu4e-maildir           . "~/Mail/tildeteam")
                     (mu4e-sent-folder       . "/Sent")
                     (mu4e-drafts-folder     . "/Drafts")
                     (mu4e-trash-folder      . "/Trash")
                     (mu4e-refile-folder     . "/archive")))))
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-draft-mark     '("D" . "⚒")
        mu4e-headers-flagged-mark   '("F" . "٭")
        mu4e-headers-flagged-mark   '("F" . "✚")
        mu4e-headers-new-mark       '("N" . "✱")
        mu4e-headers-passed-mark    '("P" . "❯")
        mu4e-headers-replied-mark   '("R" . "❮")
        mu4e-headers-seen-mark      '("S" . "✔")
        mu4e-headers-seen-mark      '("S" . "")
        mu4e-headers-trashed-mark   '("T" . "⏚")
        mu4e-headers-attach-mark    '("a" . "⚓")
        mu4e-headers-attach-mark    '("a" . "⮹")
        mu4e-headers-attach-mark    '("a" . "a")
        mu4e-headers-attach-mark    '("a" . "æ")
        mu4e-headers-attach-mark    '("a" . "@")
        mu4e-headers-encrypted-mark '("x" . "⚴")
        mu4e-headers-signed-mark    '("s" . "☡")
        mu4e-headers-unread-mark    '("u" . "⎕"))
  (setq mu4e-headers-has-child-prefix    '("+"  . "◼ "))
  (setq mu4e-headers-empty-parent-prefix '("-"  . "◽ "))
  (setq mu4e-headers-first-child-prefix  '("\\" . "┗▶"))
  (setq mu4e-headers-duplicate-prefix    '("="  . "≡ "))
  (setq mu4e-headers-default-prefix      '("|"  . "│ "))
  ;; The headers to be shown
  ;; Added :to to be able to separate between multiple accounts.
  (setq mu4e-headers-fields
        (quote
         ((:human-date . 20)
          ;; (:flags . 6)
          (:mailing-list . 10)
          (:to . 22)
          (:from . 22)
          (:thread-subject))))

;;; Mail sending setup. Please keep `init-gnus.el` in mind!
  (setq send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        message-sendmail-f-is-evil 't
        message-sendmail-extra-arguments '("--read-envelope-from")
        mu4e-sent-messages-behavior 'delete
        mu4e-user-agent-string nil)

  ;; FIX for "Duplicate UID for xx"
  (setq mu4e-change-filenames-when-moving t))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package theme-changer
  :config
  (setq custom-theme-load-path '("/home/codingquark/.emacs.d/lisp/"
                                 "/usr/share/emacs/site-lisp/elpa/modus-themes-0.11.0/"
                                 custom-theme-directory
                                 t))
  (setq calendar-location-name "Gujarat, IN")
  ;; 23.236315, 69.734109
  (setq calendar-latitude 23.23)
  (setq calendar-longitude 69.73)
  (change-theme 'modus-operandi 'modus-vivendi))

(use-package emms
  :config
  (emms-minimalistic)
  ;; Note: practically all keybindings are in lisp/init-exwm.el
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-source-file-default-directory "~/Music/"))

(use-package exwm
  :config
  (require 'time)
  (require 'battery)
  (require 'exwm-randr)
  (require 'exwm-config)
  (require 'exwm-workspace)
  (require 'exwm-systemtray)
  (require 'exwm-systemtray)
  (setq exwm-workspace-number 4)
  (setq exwm-systemtray-height 19)
  (setq exwm-systemtray-icon-gap 5)
  (setq battery-mode-line-format "[%b%p%% %L]")
  (setq display-time-string-forms '((format-time-string "%b %d - %H:%M " now)))
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "HDMI-1"
                                               2 "eDP-1" 3 "HDMI-1"
                                               4 "eDP-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output HDMI-1 --left-of eDP-1 --auto")))
  ;; (when (string= system-name "proton")
  ;;   (setq exwm-randr-workspace-output-plist '(2 "eDP1" 3 "HDMI1" 0 "eDP1"
  ;;                                               4 "eDP1" 5 "eDP1" 6 "HDMI1"
  ;;                                               7 "eDP1" 8 "eDP1" 9 "eDP1"
  ;;                                               1 "HDMI1")))
  (exwm-randr-enable)
  (exwm-systemtray-enable)
  (display-time-mode t)
  (display-battery-mode 1)
  (fringe-mode 8)
  (exwm-config-ido)
  (defun brighter ()
    (interactive)
    (start-process-shell-command "xbacklight" nil "xbacklight +5")
    (shell-command "xbacklight"))
  (defun brightest ()
    (interactive)
    (start-process-shell-command "xbacklight" nil "xbacklight -set 100")
    (shell-command "xbacklight"))
  (defun dimmer ()
    (interactive)
    (start-process-shell-command "xbacklight" nil "xbacklight -5")
    (shell-command "xbacklight"))
  (defun dimmest ()
    (interactive)
    (start-process-shell-command "xbacklight" nil "xbacklight -set 30"))
  (defun switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p  "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 'brighter)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'dimmer)
  (exwm-input-set-key (kbd "s-b") 'ivy-switch-buffer)
  (exwm-input-set-key (kbd "s-x") 'counsel-M-x)
  (exwm-input-set-key (kbd "s-o") 'other-window)
  (exwm-input-set-key (kbd "s-r") 'rename-buffer)
  (exwm-input-set-key (kbd "s-B") 'mode-line-other-buffer)

  ;; I find these useful only in exwm, but they're not bound to `s`
  (global-set-key (kbd "C-c +") 'emms-volume-mode-plus)
  (global-set-key (kbd "C-c -") 'emms-volume-mode-minus)
  (global-set-key (kbd "C-c SPC") 'emms-pause)
  (global-set-key (kbd "C-c q") 'emms-stop)

  ;; (global-set-key (kbd "s-m") 'emms)
  ;; (global-set-key (kbd "s-M") 'emms-play-url)
  ;; (global-set-key (kbd "s-<") 'emms-previous)
  ;; (global-set-key (kbd "s->") 'emms-next)

  ;; Time to have some keybindings
  (setq exwm-input-global-keys
        `(
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-n" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 exwm-workspace-number))
          ;; Bind "s-&" to launch applications ('M-&' also works if the output
          ;; buffer does not bother you).
          ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
          ([?\s-f] . (lambda ()
                       (interactive)
                       (start-process-shell-command "Firefox" "firefox" "firefox")))
          ([?\s-t] . (lambda ()
                       (interactive)
                       (start-process-shell-command "st" "st-term" "st -e tmux -u")))
          ([?\s-p] . (lambda ()
                       (interactive)
                       (start-process-shell-command "PAVUcontrol" "pavucontrol" "pavucontrol")))
          ([?\s-s] . (lambda ()
                       (interactive)
                       (start-process-shell-command "Slack" "slack" "slack")))
          ([?\s-g] . (lambda ()
                       (interactive)
                       (start-process-shell-command "Chrome" "google-chrome" "google-chrome")))
          ([?\s-e] . (lambda ()  ;; e for emails
                       (interactive)
                       (start-process-shell-command "ProtonMail" "protonmail-bridge" "protonmail-bridge")))
          ([s-f1] . (lambda ()
		      (interactive)
		      (start-process "" nil "/usr/bin/slock")))
          ([s-f2] . (lambda ()
		      (interactive)
		      (dimmest)))
          ([s-f3] . (lambda ()
		      (interactive)
		      (brightest)))
          ([?\s-M] . (lambda ()
                       (interactive)
                       ((call-interactively emms-play-url))))
          ([?\s-m] . (lambda ()
                       (interactive)
                       (emms)))
          ([?\s-<] . (lambda ()
                       (interactive)
                       (emms-previous)))
          ([?\s->] . (lambda ()
                       (interactive)
                       (emms-next)))
          ([s-return] . (lambda ()
                          (interactive)
                          (split-window-right)))))
  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ([?\C-m] . [return])
          ;; cut/paste.
          ;; ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))
  (global-set-key (kbd "C-c s") 'switch-to-scratch))




(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; set a default font
(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka Nerd Font"))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))
(set-face-attribute 'default t :font "Iosevka Nerd Font")

;; The following will change the all existing frame's fonts
(set-frame-font "Iosevka Nerd Font" t
                (frame-list))

(defun extract-titles-from-arxiv ()
  "Extract titles of papers from arxiv subscription emails.
Uses `occur` in doing so with a regexp."
  (interactive)
  (occur "^Title:.*\\Ca.*Authors"))

(require 'init-alarm)
(require 'custom)
;; (require 'init-exwm)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (setq package-check-signature nil)

;; (exwm-enable)

;; (server-start)

(put 'list-timers 'disabled nil)
;;; init.el ends here
