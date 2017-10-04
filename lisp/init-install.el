;; Install missing things

(require 'cl)

;; install these automatically if missing
(defvar my-packages
  '(auto-complete
    better-defaults
    emms
    ido-ubiquitous
    jedi
    magit
    markdown-mode
    popup
    pydoc-info
    elpy
    resize-window
    smartparens
    smex
    switch-window
    smtpmail-multi
    php-mode
    php-auto-yasnippets
    flymake-php
    diff-hl
    web-mode
    flycheck
    phpunit
    auto-complete
    restclient
    dired+
    auto-highlight-symbol
    js2-mode
    json-mode
    angular-mode
    apache-mode
    lorem-ipsum
    multiple-cursors
    regex-tool
    projectile
    org-bullets
    pdf-tools
    arch-packer
    elfeed
    w3m
    stumpwm-mode
    grizzl)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'init-install)
