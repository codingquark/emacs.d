;; Taken from https://lists.gnu.org/archive/html/help-gnu-emacs/2006-06/msg00382.html

(defvar alarm-clock-timer nil
  "Keep timer so that the user can cancel the alarm.")

(defun alarm-clock-message (text)
  "The alarm is notified about showing TEXT."
  (notifications-notify
   :title "Alarm!"
   :body text))

(defun alarm-clock ()
  "Set an alarm.
The time format is the same accepted by `run-at-time'.  For
example \"11:30am\"."
  (interactive)
  (let ((time (read-string "Time: "))
        (text (read-string "Alarm message: ")))
    (setq alarm-clock-timer (run-at-time time nil 'alarm-clock-message text))))

(defun alarm-clock-cancel ()
  "Cancel the alarm clock."
  (interactive)
  (cancel-timer alarm-clock-timer))

(provide 'init-alarm)
;;; init-alarm.el ends here
