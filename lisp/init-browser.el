;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'w3m)
(require 'eww)
(require 'browse-url)

(setq browse-url-browser-function '(("\\(wikipedia.org\\)\\|\\(sprunge.us\\)" . eww-browse-url)
                                    (".*" . browse-url-firefox)))
;; Set ddg as default search engine
(setq w3m-search-default-engine "duckduckgo")
;; Use this if you want to search wikipedia all the time
;; (setq eww-search-prefix "https://en.wikipedia.org/wiki/Special:Search?search=")
;; DDG for other cases
(setq eww-search-prefix "https://duckduckgo.com/html/?q=")

;; Custom key-bindings
(global-set-key (kbd "C-c s") 'w3m-search)

(provide 'init-browser)
;;; init-browser.el ends here
