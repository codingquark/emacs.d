;;; package --- Summary:
;;; Commentary:

;;; Code:
(require 'org-bullets)
(setq org-directory "~/Documents/org")
(setq org-archive-location "~/Documents/org/archive.org::* From %s")
(setq org-default-notes-file (concat org-directory "/index.org"))
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo/tasks.org") "Tasks")
         "* TODO %?\n %i\n %a")
        ("s" "Shopping" entry (file+headline (concat org-directory "/todo/shopping.org") "List")
         "* TODO %?\n %i")
        ("p" "Paper" entry (file+headline (concat org-directory "/research/papers.org") "Links")
         "* TODO %A")))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(provide 'init-org)
;;; init-org ends here
