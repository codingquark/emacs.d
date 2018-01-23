(require 'elfeed)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '("http://planet.emacsen.org/atom.xml"
        "http://codingquark.com/feed.xml"
        ;; "https://medium.com/feed/@abrahamv23" -- 404
        "http://blog.stephenwolfram.com/feed/"
        "https://www.lightbluetouchpaper.org/feed/"
        ;; "https://tanninpower.wordpress.com/feed/" -- out of order for unknown time
        "http://blog.jaysinh.com/feed.xml"
        "esr.ibiblio.org/?feed=rss2"
        "http://idevji.com/feed"
        "http://www.shakthimaan.com/news.xml"))

(provide 'init-elfeed)
