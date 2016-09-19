(require 'elfeed)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '("http://feeds.feedburner.com/sathyabh/at"
        "http://feeds.feedburner.com/SathyaSays"
        "http://planet.emacsen.org/atom.xml"
        "http://codingquark.com/feed.xml"
        "https://medium.com/feed/@abrahamv23"
        "http://blog.stephenwolfram.com/feed/"
        "https://www.lightbluetouchpaper.org/feed/"))

(provide 'init-elfeed)
