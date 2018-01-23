;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'w3m)
(require 'browse-url)

(setq browse-url-browser-function '(("\\(wikipedia.org\\)\\|\\(sprunge.us\\)" . w3m-browse-url)
                                    (".*" . browse-url-firefox)))
;; Set ddg as default search engine
(setq w3m-search-default-engine "duckduckgo")

;; Custom key-bindings
(global-set-key (kbd "C-c s") 'w3m-search)

(provide 'init-browser)
;;; init-browser.el ends here
