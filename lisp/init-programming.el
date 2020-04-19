;;(require 'auto-highlight-symbol)

(global-set-key (kbd "C-c g") 'rgrep)

(diff-hl-mode 1)
(diff-hl-dired-mode 1)
(global-auto-revert-mode 1)

;; (setq-default line-spacing 5) ;; Checkout `line-height` instead
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; set a default font
(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka Nerd Font"))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))
(set-face-attribute 'default t :font "Iosevka Nerd Font")

;; The following will change the all existing frame's fonts
(set-frame-font "Iosevka Nerd Font" t
                (frame-list))

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(defun extract-titles-from-arxiv ()
  "Extract titles of papers from arxiv subscription emails.
Uses `occur` in doing so with a regexp."
  (interactive)
  (occur "^Title:.*\\Ca.*Authors"))

(provide 'init-programming)
;;; init-programming ends here
