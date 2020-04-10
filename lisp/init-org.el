;;; package --- Summary:
;;; Commentary:

;;; Code:
(require 'org-bullets)
(require 'org-brain)

;; Custom set variables
(setq org-directory "~/Documents/org")
(setq org-archive-location "~/Documents/org/archive.org::* From %s")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list (concat org-directory "/todo.org")))
(setq org-deadline-warning-days 14)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-reverse-note-order t)
(setq org-tags-column -80)
(setq org-agenda-restore-windows-after-quit 1)
(setq org-agenda-custom-commands
      (quote (("d" todo "DELEGATED" nil)
              ("c" todo "DONE|DEFERRED|CANCELLED" nil)
              ("w" todo "WAITING" nil)
              ("W" agenda "" ((org-agenda-ndays 21)))
              ("A" agenda ""
               ((org-agenda-skip-function
                 (lambda nil
                   (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
                (org-agenda-ndays 1)
                (org-agenda-overriding-header "Today's Priority #A tasks: ")))
              ("u" alltodo ""
               ((org-agenda-skip-function
                 (lambda nil
                   (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                             (quote regexp) "\n]+>")))
                (org-agenda-overriding-header "Unscheduled TODO entries: "))))))

;; Key bindings
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "DEFERRED(f)"
         "DELEGATED(l)"
         "STARTED(s)"
         "WAITING(w)"
         "|"
         "DONE(d)"
         "CANCELLED(x)")))

(setq org-capture-templates
      '(("t" "Todo" entry (file (lambda () (concat org-directory "/todo.org")))
         "* TODO %? %^G\n %i\n %a %u")
        ("n" "Note" entry (file+headline (lambda () (concat org-directory "/notes.org")) "Notes")
         "* %?\n %i\n %u %^G")
        ("s" "Schedule toots" plain (file "~/Documents/org/scheduled_toots.org")
         "%?%i")
        ("b" "Brain" plain (function org-brain-goto-end)
         "* %i%?" :empty-lines 1)))

;; Various hooks
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(defun auto-fill-org-mode-hook()
  (progn
    (turn-on-flyspell)
    (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'auto-fill-org-mode-hook)

;; Large LaTeX previews
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
;; Do not export with sub-super-scripts
(setq org-export-with-sub-superscripts nil)


;; Time to setup org-brain
(setq org-brain-path (concat org-directory "/brain")) ;; No need to do that as it defaults to org-directory/brain
(setq org-id-track-globally t)  ;; this is t by default, merely making sure
(setq org-id-locations-file (concat user-emacs-directory "/.org-id-locations"))
(setq org-brain-show-resources t)  ;; Clean org-brain-visualize
(global-set-key (kbd "C-c b") 'org-brain-visualize)

(provide 'init-org)
;;; init-org ends here
