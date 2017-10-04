;;; package --- Summary
;;; Commentary:

;;; Code:

(require 'w3m)
(require 'browse-url)

(setq browse-url-browser-function '(("\\(wikipedia.org\\)\\|\\(sprunge.us\\)" . w3m-browse-url)
                                    (".*" . browse-url-firefox)))

(provide 'init-browser)
;;; init-browser.el ends here
