(require 'elfeed)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '("http://feeds.feedburner.com/sathyabh/at"
        "http://feeds.feedburner.com/SathyaSays"))

(provide 'init-elfeed)
