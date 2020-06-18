(require 'mu4e)

;; We definitely need a keybinding now.
(global-set-key (kbd "C-c m") 'mu4e)


;; Let's fetch the mails!
;; No need to run `mu4e-update-index` periodically.
(setq mu4e-get-mail-command "/usr/bin/offlineimap -o -u syslog"
      mu4e-update-interval (* 3 60)
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

;; Time to decide how the mails are displayed in various views.
;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
;; (setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
;; (setq mu4e-compose-context-policy nil)

(setq mu4e-maildir "~/Mail"
      mu4e-sent-folder "/Codingquark/Sent"
      mu4e-drafts-folder "/Codingquark/Drafts"
      mu4e-trash-folder "/Codingquark/Trash"
      mu4e-refile-folder "/Codingquark/archive")

(setq mu4e-user-mail-address-list '("dhavan@oviyum.com" "quark@codingquark.com"))


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
                   (mu4e-sent-folder       . "/Codingquark/Sent")
                   (mu4e-drafts-folder     . "/Codingquark/Drafts")
                   (mu4e-trash-folder      . "/Codingquark/Trash")
                   (mu4e-refile-folder     . "/Codingquark/archive")))
         ,(make-mu4e-context
           :name "Work"
           :enter-func (lambda () (mu4e-message "Entering Work context"))
           :leave-func (lambda () (mu4e-message "Leaving Work context"))
           ;; Wee match based on the contact-fields of the message
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "dhavan@oviyum.com")))
           :vars '((user-mail-address      . "dhavan@oviyum.com")
                   (smtpmail-smtp-user     . "dhavan@oviyum.com")
                   (user-full-name         . "Dhavan V")
                   (mu4e-compose-signature .
                                           (concat
                                            "Dhavan"))
                   (mu4e-sent-folder       . "/Oviyum/Sent")
                   (mu4e-drafts-folder     . "/Oviyum/Drafts")
                   (mu4e-trash-folder      . "/Oviyum/Trash")
                   (mu4e-refile-folder     . "/Oviyum/archive")))))

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
(setq mu4e-header-fields
      '((:human-date . 8)
        (:flags . 6)
        (:mailing-list . 10)
        (:from . 22)
        (:to . 22)
        (:subject)))

;;; Mail sending setup. Please keep `init-gnus.el` in mind!
(setq send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      message-sendmail-f-is-evil 't
      message-sendmail-extra-arguments '("--read-envelope-from"))

(provide 'init-mu4e)
