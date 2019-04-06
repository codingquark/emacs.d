(require 'elfeed)

(defvar cq/youtube-dl-path)
(defvar cq/youtube-dl-output-dir)

(global-set-key (kbd "C-x w") 'elfeed)

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))

(setq cq/youtube-dl-path "/usr/bin/youtube-dl")
(setq cq/youtube-dl-output-dir "~/Videos/")

(defun cq/elfeed-download-video ()
  "Download a video using youtube-dl."
  (interactive)
  (async-shell-command (format "%s -o \"%s%s\" -f bestvideo+bestaudio %s"
                               cq/youtube-dl-path
                               cq/youtube-dl-output-dir
                               "%(title)s.%(ext)s"
                               (elfeed-entry-link elfeed-show-entry))))

(setq elfeed-feeds
      '(;; "http://planet.emacsen.org/atom.xml"
        "http://codingquark.com/feed.xml"
        "http://blog.stephenwolfram.com/feed/"
        "https://www.lightbluetouchpaper.org/feed/"
        ;; "https://tanninpower.wordpress.com/feed/" -- out of order for unknown time
        "http://blog.jaysinh.com/feed.xml"
        ;; "esr.ibiblio.org/?feed=rss2"
        "http://idevji.com/feed"
        "http://jordi.inversethought.com/feed/"
        "http://www.shakthimaan.com/news.xml"
        "https://static.fsf.org/fsforg/rss/blogs.xml"
        "http://technomancy.us/feed/atom.xml"
        "http://www.aidalgolland.net/feed.xml"
        "http://www.dijkstrascry.com/rss.xml"
        "https://binaryredneck.net/rss/"
        "https://ftfl.ca/blog/index.rss"
        "http://planet.gentoo.org"
        "https://lwn.net/headlines/rss"))

(provide 'init-elfeed)
