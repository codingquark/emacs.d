(require 'elfeed)
(require 'elfeed-protocol)

(setq elfeed-use-curl nil)
(elfeed-set-timeout 36000)

(defvar cq/youtube-dl-path)
(defvar cq/youtube-dl-output-dir)

(global-set-key (kbd "C-x w") 'elfeed)

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))

(setq cq/youtube-dl-path "/home/codingquark/programs/ytdl/youtube-dl")
(setq cq/youtube-dl-output-dir "~/Videos/")

;; Schedule feed update for every day at 3PM
(run-at-time "15:00" nil 'elfeed-update)

(defun cq/elfeed-download-video ()
  "Download a video using youtube-dl."
  (interactive)
  (async-shell-command (format "%s -o \"%s%s\" -f bestvideo+bestaudio %s"
                               cq/youtube-dl-path
                               cq/youtube-dl-output-dir
                               "%(title)s.%(ext)s"
                               (elfeed-entry-link elfeed-show-entry))))

(setq elfeed-protocol-ttrss-maxsize 200) ; bigger than 200 is invalid
(setq elfeed-feeds
      '(
        ("youwannaknowright"
         :password (shell-command-to-string "pass --clip personal/Root/Important/Freedombox")
         )))

(setq elfeed-log-level 'debug)

;; (setq elfeed-feeds
;;       '(;; "http://planet.emacsen.org/atom.xml"
;;         ("http://planet.emacslife.com/atom.xml" emacs)
;;         "http://codingquark.com/feed.xml"
;;         "http://blog.stephenwolfram.com/feed/"
;;         "https://www.lightbluetouchpaper.org/feed/"
;;         "http://blog.jaysinh.com/feed.xml"
;;         "esr.ibiblio.org/?feed=rss2"
;;         "http://idevji.com/feed"
;;         "http://jordi.inversethought.com/feed/"
;;         "http://www.shakthimaan.com/news.xml"
;;         "https://static.fsf.org/fsforg/rss/blogs.xml"
;;         "http://technomancy.us/feed/atom.xml"
;;         "http://www.aidalgolland.net/feed.xml"
;;         "http://www.dijkstrascry.com/rss.xml"
;;         "https://binaryredneck.net/rss/"
;;         "https://ftfl.ca/blog/index.rss"
;;         "http://planet.gentoo.org"
;;         "https://lwn.net/headlines/rss"
;;         "https://cjb.sh/articles/feed.xml"
;;         "https://rjlipton.wordpress.com/feed/"
;;         ;; "https://www.jwz.org/blog/feed"
;;         "http://www.antipope.org/charlie/blog-static/atom.xml"
;;         "https://usesthis.com/feed.atom"
;;         "https://blog.liw.fi/index.atom"
;;         "http://www.earth.li/~noodles/blog/feed.xml"
;;         "http://0pointer.net/blog/index.rss20"
;;         "kushaldas.in/rss.xml"
;;         "http://ebb.org/bkuhn/blog/rss.xml"

;;         ;; PODCASTS
;;         ;; ("http://podcasts.joerogan.net/feed" podcasts) ;; joe rogan experience
;;         ;; ("https://lexfridman.com/category/ai/feed/" podcasts)
;;         ))

(elfeed-protocol-enable)

(provide 'init-elfeed)
