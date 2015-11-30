(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(require 'cl)

;; install these automatically if missing
;; TODO: add python related packages
(defvar my-packages
  '(better-defaults
    god-mode
    ido-ubiquitous
    magit
    resize-window
    smartparens
    smex
    switch-window
    twittering-mode
    python-mode)
  "A list of packages to ensure are installed at launch")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


(load-theme 'peacock t) ;; static for now, load it from somewhere else later.
(require 'better-defaults)
(erc-autojoin-mode t)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; god-mode - tries to avoid RSI
(global-set-key (kbd "<escape>") 'god-mode-all)

;; better window switching
(require 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)

;; ERC hook.
;; connects to freenode, identifies, joins channels,
(add-hook 'erc-mode-hook
	  (lambda()
	    ;; disable smartparens, because no one on IRC is smart
	    (smartparens-mode 0)
	    ))
(add-hook 'erc-after-connect '(lambda (SERVER NICK)
                                (erc-message "PRIVMSG" "NickServ identify YOUR_PASSWD"))) ;; load password from elsewhere

(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs")))

(require 'erc-match)
(setq erc-keywords '("codingquark"))
(erc-match-mode)

(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))

;; Truncate buffers so they don't hog core.
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)
;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)

;;; Finally, connect to the networks.
(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667
         :nick "codingquark" :full-name "Dhavan")))

;; initialize smex for M-x awesomeness
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; We want ido-mode everywhere!
;; First, enable ido itself. Then go for ido-ubiquitous
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Smartparens
(require 'smartparens)
(smartparens-global-mode t)

;;; init-local.el ends here
