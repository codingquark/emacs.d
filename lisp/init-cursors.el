(require 'multiple-cursors)

;; Add one cursor to each marked line
(global-set-key (kbd "C-s-c C-s-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'init-cursors)
