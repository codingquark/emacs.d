(require 'mastodon)
(setq mastodon-instance-url "https://mathstodon.xyz")
;; (setq mastodon-auth-source-file "~/.authinfo.gpg")
;; (setq mastodon-auth-source-file "")
;; (setq mastodon-auth-mechanism 'oauth2)


(defun cq/read-one-toot (file-path)
  "Return a list of lines of a file at FILE-PATH.
Format is <toot>\n---\n<toot>..."
  (with-temp-buffer
    (insert-file-contents file-path)
    (car (split-string (buffer-string) "---" t))))

(defun cq/erase-one-toot (file-path)
  "Erase a single toot from FILE-PATH file."
  (let ((all-toots (with-temp-buffer
                     (insert-file-contents file-path)
                     (buffer-string))))
    (with-temp-file file-path
      (insert (mapconcat 'identity
                         (cdr (split-string all-toots "---" t))
                         "---")))))

(defun cq/send-tweet-and-toot (toot)
  "Start processes to send TOOT to twitter and mastodon."
  (interactive "sContent: ")
  (start-process "oysttyer" "*scheduled-tooter*" "oysttyer"
                 (concat "-status=" toot)
                 "-script")
  (start-process "toot" "*scheduled-tooter*" "toot"
                 "post" toot)
  (message "Sent."))

(defun tweet-and-toot (file-path)
  "Reading from FILE-PATH, post to twitter and mastodon.
Makes sure that the file is not empty."
  (let ((toot (string-trim-right (cq/read-one-toot file-path))))
    (when (not (or (string= toot "")
                   (string= toot nil)))
      (cq/send-tweet-and-toot toot)
      (cq/erase-one-toot file-path))))

;; Post every 10min
(defun schedule-tweet-and-toot ()
  "Schedule a function that posts to twitter and mastodon every 5 minutes."
  (run-with-timer 0 (* 60 5) 'tweet-and-toot "~/Documents/org/scheduled_toots.org"))

(schedule-tweet-and-toot)


(provide 'init-mastodon)
