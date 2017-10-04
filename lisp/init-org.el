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
      '(("t" "Todo" entry (file+headline (lambda () (concat org-directory "/todo/tasks.org")) "Tasks")
         "* TODO %?\n %i\n %a")
        ("s" "Shopping" entry (file+headline (lambda () (concat org-directory "/todo/shopping.org")) "List")
         "* TODO %?\n %i")
        ("p" "Paper" entry (file+headline (lambda () (concat org-directory "/research/papers.org")) "Links")
         "* TODO %A")))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; remove comments from org document for use with export hook
;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
(defun delete-org-comments (backend)
  ;;; remove comments from org document for use with export hook
  (loop for comment in (reverse (org-element-map (org-element-parse-buffer)
                    'comment 'identity))
    do
    (setf (buffer-substring (org-element-property :begin comment)
                (org-element-property :end comment))
          "")))

;; add to export hook
(add-hook 'org-export-before-processing-hook 'delete-org-comments)

(provide 'init-org)
;;; init-org ends here
