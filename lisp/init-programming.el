;;(require 'auto-highlight-symbol)

(global-set-key (kbd "C-c g") 'rgrep)

(diff-hl-mode 1)
(diff-hl-dired-mode 1)
(global-auto-revert-mode 1)

;; (setq-default line-spacing 5) ;; Checkout `line-height` instead

;; (set-face-attribute 'default t :font "Source Code Pro-11")
(set-face-attribute 'default t :font "iosevka curly extended-11")
;; The following will change the current frame's fonts
;; (set-frame-font "Iosevka Curly Extended-11" nil t)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; (add-to-list 'default-frame-alist '(font . "Source Code Pro-11" ))
(add-to-list 'default-frame-alist '(font . "iosevka curly extended-11" ))

;; (set-fontset-font t nil (font-spec :size 11 :name "Noto"))

(defun extract-titles-from-arxiv ()
  "Extract titles of papers from arxiv subscription emails.
Uses `occur` in doing so with a regexp."
  (interactive)
  (occur "^Title:.*\\Ca.*Authors"))

(provide 'init-programming)
;;; init-programming ends here
