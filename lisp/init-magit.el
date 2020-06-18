(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

;; Hide these sections in status buffer because they may cause emacs
;; to hang-up when the content is large
(setq magit-section-initial-visibility-alist
      '((stashes . hide) (untracked . hide) (unpushed . hide)))

(provide 'init-magit)
