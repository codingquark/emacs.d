;; erc config
(require 'secrets)
(erc-autojoin-mode t)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; god-mode - tries to avoid RSI
(global-set-key (kbd "<escape>") 'god-mode-all)

;; better window switching
(require 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)

;; ERC hook.
;; connects to freenode, identifies, joins channels,
(add-hook 'erc-mode-hook
	  (lambda()
	    ;; disable smartparens, because no one on IRC is smart
	    (smartparens-mode 0)
	    ))
(add-hook 'erc-after-connect '(lambda (SERVER NICK)
                                (erc-message "PRIVMSG" (concat "NickServ identify " secret-erc-password)))) ;; load password from elsewhere

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

(provide 'init-erc)
