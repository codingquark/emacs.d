;;
;; GNUS customizations
;;

(require 'gnus)
(require 'nnir)
(require 'smtpmail)
(require 'smtpmail-multi)
(require 'epa-file)
(load "quark-gnus-auto.el")
(setq user-full-name "Dhavan Vaidya")

(setq gnus-select-method '(nnnil ""))

;; default sending method - using internal smtp client
;; with the smtpmail-multi package to handle multiple email
;; accounts
(setq message-send-mail-function 'smtpmail-multi-send-it)

;; and workaround for Gmail folders
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; verbose printing of SMTP issues
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verbose t)

;; set gnus-parameter
(setq gnus-parameters
  '(("nnimap.*"
     (gnus-use-scoring nil)
     (expiry-wait . 2)
     (display . all))))

;;[[http://stackoverflow.com/questions/4982831/i-dont-want-to-expire-mail-in-gnus]]
(setq gnus-large-newsgroup 'nil)


;; organize groups by topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; sort threads
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

;; organize in threads
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; cache for offline reading
(setq gnus-use-cache t)

;; Check for mails every 1 min
(gnus-demon-add-handler 'gnus-demon-scan-news 1 t)
(gnus-demon-init)
(add-hook 'gnus-summary-exit-hook 'gnus-notify+)
(add-hook 'gnus-group-catchup-group-hook 'gnus-notify+)
(add-hook 'mail-notify-pre-hook 'gnus-notify+)

;; If we go offline, demon would crash. Prevent that.
(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (120 (message "Gnus timed out."))
    ad-do-it))

;; Holy crap! This mode will show time
;; And whenver some email arrives, it'll show a tiny icon nerby!
;; Does not check for subdirectories, so I've kept it only for
;; important stuff.
;; (setq display-time-format "")
(setq display-time-mail-directory "~/Mail/Oviyum/INBOX/new")
(setq read-mail-command 'gnus)
(setq display-time-interval 30)
(display-time-mode)
(add-hook 'gnus-group-mode-hook 'display-time-event-handler)
(gnus-select-account-enable)

(provide 'init-gnus)
