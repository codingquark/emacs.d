(require 'exwm)
(require 'exwm-config)
(require 'exwm-systemtray)

(setq exwm-workspace-number 4)

(require 'exwm-systemtray)
(exwm-systemtray-enable)
(display-time-mode t)
(display-battery-mode 1)
(setq battery-mode-line-format "[%b%p%% %L]")
(setq display-time-string-forms '((format-time-string "%b %d - %H:%M " now)))

;; (setq display-time-default-load-average nil)
;; (ido-mode 1)
(fringe-mode 1)
(exwm-config-ido)
;; (mouse-avoidance-mode "banish")

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p  "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))

(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(defun brighter ()
  (interactive)
  (start-process-shell-command "xbacklight" nil "xbacklight +5")
  (shell-command "xbacklight"))
(defun brightest ()
  (interactive)
  (start-process-shell-command "xbacklight" nil "xbacklight -set 100")
  (shell-command "xbacklight"))
(defun dimmer ()
  (interactive)
  (start-process-shell-command "xbacklight" nil "xbacklight -5")
  (shell-command "xbacklight"))
(defun dimmest ()
  (interactive)
  (start-process-shell-command "xbacklight" nil "xbacklight -set 30")
  (shell-command "xbacklight"))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 'brighter)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'dimmer)
(exwm-input-set-key (kbd "s-b") 'ivy-switch-buffer)
(exwm-input-set-key (kbd "s-x") 'counsel-M-x)
(exwm-input-set-key (kbd "s-o") 'other-window)
(exwm-input-set-key (kbd "s-r") 'rename-buffer)
(exwm-input-set-key (kbd "s-B") 'mode-line-other-buffer)

;; Time to have some keybindings
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-6" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 4))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
        ([?\s-f] . (lambda ()
                     (interactive)
                     (start-process-shell-command "Firefox" "firefox" "firefox")))
        ([?\s-t] . (lambda ()
                     (interactive)
                     (start-process-shell-command "URxvt" "urxvt" "urxvt")))
        ([?\s-p] . (lambda ()
                     (interactive)
                     (start-process-shell-command "PAVUcontrol" "pavucontrol" "pavucontrol")))
        ([?\s-s] . (lambda ()
                     (interactive)
                     (start-process-shell-command "Slack" "slack" "slack")))
        ([?\s-g] . (lambda ()
                     (interactive)
                     (start-process-shell-command "Chrome" "google-chrome" "google-chrome")))
        ([s-f1] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/slock")))
        ([s-f2] . (lambda ()
		    (interactive)
		    (dimmest)))
        ([s-f3] . (lambda ()
		    (interactive)
		    (brightest)))))

(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(0 "eDP1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output HDMI1 --right-of eDP1 --auto")
            (setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "HDMI1"
                                                        2 "eDP1" 3 "HDMI1"
                                                        4 "eDP1"))))
(exwm-randr-enable)
;; (when (string= system-name "proton")
;;   (require 'exwm-randr)
;;   (setq exwm-randr-workspace-output-plist '(2 "eDP1" 3 "HDMI1" 0 "eDP1"
;;                                               4 "eDP1" 5 "eDP1" 6 "HDMI1"
;;                                               7 "eDP1" 8 "eDP1" 9 "eDP1"
;;                                               1 "HDMI1")))

(provide 'init-exwm)
