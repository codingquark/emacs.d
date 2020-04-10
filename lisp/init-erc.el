;; erc config
(require 'znc)
(require 'secrets)
;; (require 'erc-scrolltoplace)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
;; (setq erc-hide-list '())
(setq erc-paranoid t)
;; (add-to-list 'erc-modules 'scrolltoplace)
;; (erc-update-modules)

;; ERC hook.
;; connects to freenode, identifies, joins channels,
(add-hook 'erc-mode-hook
          (lambda()
            ;; disable smartparens, because no one on IRC is smart
            (smartparens-mode 0)))
;; (add-hook 'erc-after-connect '(lambda (SERVER NICK)
;;                                 (erc-message "PRIVMSG" (concat "quote pass codingquark:" secret-erc-password)))) ;; load password from elsewhere

(require 'erc-join)
(erc-autojoin-mode 1)
;; Channels list is handled by znc
;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#emacs" "##penfiends")
;;         ("tilde.chat" "#cosmic")
;;         ("oftc.net" "#debian" "#debian-in" "#debian-emacs" "#debian-au")))

(require 'erc-match)
(setq erc-keywords '("codingquark"))
(setq erc-modules (quote
              (autojoin button completion fill irccontrols keep-place list log match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp spelling track)))
(erc-match-mode)
(setq erc-pals
      (quote
       ("twb" "thebigj" "technomancy" "forcer" "wasamasa" "parjanya" "JordiGH" "parsnip" "oshor" "mbuf" "aidalgol" "bremner")))
(setq erc-timestamp-intangible t)
(setq erc-timestamp-right-column 80)

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
(setq erc-part-reason 'erc-part-reason-normal)
(setq erc-quit-reason 'erc-quit-reason-normal)
;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)

;; Jump to #emacs, we do that a lot.
(global-set-key (kbd "C-c i e") (lambda () (interactive) (switch-to-buffer "#emacs")))

(provide 'init-erc)
