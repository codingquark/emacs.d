;; Install missing things

(require 'cl)

;; install these automatically if missing
(defvar my-packages
  '(auto-complete
    better-defaults
    emms
    god-mode
    ido-ubiquitous
    jedi
    magit
    popup
    pydoc-info
    elpy
    resize-window
    smartparens
    smex
    switch-window
    twittering-mode)
  "A list of packages to ensure are installed at launch")

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
