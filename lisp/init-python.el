;; elpy
(require 'elpy)
(elpy-enable)
(setq python-shell-exec-path '("/usr/bin/python3"))
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3")

;; Anaconda setup
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(provide 'init-python)
