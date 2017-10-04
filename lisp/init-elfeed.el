(require 'elfeed)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '("http://planet.emacsen.org/atom.xml"
        "http://codingquark.com/feed.xml"
        "https://medium.com/feed/@abrahamv23"
        "http://blog.stephenwolfram.com/feed/"
        "https://www.lightbluetouchpaper.org/feed/"
        "https://tanninpower.wordpress.com/feed/"
        "http://blog.jaysinh.com/feed.xml"))

(provide 'init-elfeed)