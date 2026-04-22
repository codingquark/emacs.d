;;; cq-home-assistant.el --- Home Assistant integration for Emacs  -*- lexical-binding: t; -*-

;; Author: codingquark
;; Description: Fetch Home Assistant entity state inside Emacs.

;;; Commentary:

;; Talk to the local Home Assistant REST API from inside Emacs.
;; The token is expected in `~/.authinfo.gpg':
;;
;;   machine homeassistant.lan login homeassistant password YOUR_TOKEN
;;
;; The main entry point is the global minor mode `cq-home-assistant-mode',
;; which displays the configured entity state in the mode line and refreshes
;; it periodically.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'subr-x)
(require 'url-http)

(defgroup cq-home-assistant nil
  "Fetch Home Assistant entity state inside Emacs."
  :group 'applications)

(defcustom cq-home-assistant-base-url "http://homeassistant.lan:8123"
  "Base URL for the local Home Assistant instance."
  :type 'string)

(defcustom cq-home-assistant-auth-source-host "homeassistant.lan"
  "Auth-source host used to look up the Home Assistant token."
  :type 'string)

(defcustom cq-home-assistant-auth-source-user "homeassistant"
  "Auth-source user used to look up the Home Assistant token."
  :type 'string)

(defcustom cq-home-assistant-entity-id
  "sensor.weatherstation_ground_bme280_bme280_temperature"
  "Default Home Assistant entity id shown by `cq-home-assistant-mode'."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom cq-home-assistant-known-entities
  '("sensor.weatherstation_ground_bme280_bme280_temperature")
  "Known Home Assistant entity ids offered for completion."
  :type '(repeat string))

(defcustom cq-home-assistant-mode-line-label nil
  "Short label to show in the mode line instead of the friendly entity name."
  :type '(choice (const :tag "Use Home Assistant name" nil) string))

(defcustom cq-home-assistant-refresh-interval 900
  "Seconds between automatic Home Assistant refreshes."
  :type 'integer)

(defvar cq-home-assistant--mode-line "")
(defvar cq-home-assistant--timer nil)
(defvar cq-home-assistant--request-in-flight nil)

(defvar cq-home-assistant-prefix-map (make-sparse-keymap)
  "Prefix keymap for Home Assistant commands.")

(defconst cq-home-assistant--mode-line-form
  '(:eval
    (and cq-home-assistant-mode
         (not (string-empty-p cq-home-assistant--mode-line))
         cq-home-assistant--mode-line)))

(defun cq-home-assistant--token ()
  "Return the Home Assistant long-lived access token from auth-source."
  (let* ((match (car (auth-source-search
                      :max 1
                      :host cq-home-assistant-auth-source-host
                      :user cq-home-assistant-auth-source-user
                      :require '(:secret))))
         (secret (plist-get match :secret)))
    (unless secret
      (user-error "No Home Assistant token found for %s/%s"
                  cq-home-assistant-auth-source-host
                  cq-home-assistant-auth-source-user))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun cq-home-assistant--api-url (path)
  "Build a Home Assistant API URL from PATH."
  (format "%s%s"
          (string-remove-suffix "/" cq-home-assistant-base-url)
          path))

(defun cq-home-assistant--current-entity ()
  "Return the configured default Home Assistant entity id."
  (or cq-home-assistant-entity-id
      (user-error "Set `cq-home-assistant-entity-id` first")))

(defun cq-home-assistant--read-entity-id ()
  "Prompt for a Home Assistant entity id."
  (let ((default cq-home-assistant-entity-id))
    (completing-read
     (if default
         (format "Home Assistant entity (%s): " default)
       "Home Assistant entity: ")
     cq-home-assistant-known-entities
     nil
     nil
     nil
     nil
     default)))

(defun cq-home-assistant--request-state-callback (status entity-id callback error-callback)
  "Handle an async Home Assistant response for ENTITY-ID."
  (when (buffer-live-p (current-buffer))
    (unwind-protect
        (condition-case err
            (let ((transport-error (plist-get status :error)))
              (if transport-error
                  (when error-callback
                    (funcall error-callback
                             (format "Could not reach %s: %s"
                                     cq-home-assistant-base-url
                                     transport-error)))
                (let ((http-status (or url-http-response-status 0)))
                  (goto-char (point-min))
                  (re-search-forward "\r?\n\r?\n" nil 'move)
                  (if (= http-status 200)
                      (funcall callback
                               entity-id
                               (json-parse-buffer
                                :object-type 'alist
                                :array-type 'list
                                :null-object nil
                                :false-object :false))
                    (when error-callback
                      (funcall error-callback
                               (format "Home Assistant request failed with HTTP %s"
                                       http-status)))))))
          (error
           (when error-callback
             (funcall error-callback (error-message-string err)))))
      (ignore-errors (kill-buffer (current-buffer))))))

(defun cq-home-assistant--request-state-async (entity-id callback &optional error-callback)
  "Fetch Home Assistant state for ENTITY-ID and call CALLBACK.
A 10-second timer kills the request if it has not completed."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (cq-home-assistant--token)))
            ("Content-Type" . "application/json")))
         (buffer (url-retrieve
                  (cq-home-assistant--api-url
                   (format "/api/states/%s" (url-hexify-string entity-id)))
                  #'cq-home-assistant--request-state-callback
                  (list entity-id callback error-callback)
                  t t)))
    (when buffer
      (run-at-time 10 nil
                   (lambda (buf err-cb)
                     (when (buffer-live-p buf)
                       (ignore-errors
                         (let ((proc (get-buffer-process buf)))
                           (when proc (delete-process proc)))
                         (kill-buffer buf))
                       (when err-cb
                         (funcall err-cb "Home Assistant request timed out"))))
                   buffer error-callback))))

(defun cq-home-assistant--format-state (entity-id payload)
  "Return a compact display string for ENTITY-ID using PAYLOAD."
  (let* ((attributes (alist-get 'attributes payload))
         (state (alist-get 'state payload))
         (unit (alist-get 'unit_of_measurement attributes)))
    (cond
     ((and (stringp state)
           (string-match-p
            "\\`[-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\'" state)
           (stringp unit)
           (not (string-empty-p unit)))
      (format "%.2f%s" (string-to-number state) unit))
     ((and (stringp unit)
           (not (string-empty-p unit)))
      (format "%s%s" state unit))
     (t
      (format "%s: %s"
              (or cq-home-assistant-mode-line-label
                  (alist-get 'friendly_name attributes)
                  entity-id)
              state)))))

(defun cq-home-assistant--set-mode-line (text)
  "Update the Home Assistant mode line string to TEXT."
  (setq cq-home-assistant--mode-line (format " [%s]" text))
  (force-mode-line-update t))

(defun cq-home-assistant--show-buffer-callback (entity-id payload)
  "Display ENTITY-ID and PAYLOAD in a dedicated Home Assistant buffer."
  (let ((text (cq-home-assistant--format-state entity-id payload)))
    (with-current-buffer (get-buffer-create "*Home Assistant*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format-time-string "%Y-%m-%d %H:%M:%S  "))
        (insert text)
        (newline)
        (special-mode)
        (local-set-key (kbd "g") #'cq-home-assistant-show-default-buffer))
      (pop-to-buffer (current-buffer)))
    (message "%s" text)))

(defun cq-home-assistant--message-state-callback (entity-id payload)
  "Echo ENTITY-ID and PAYLOAD in the minibuffer."
  (message "%s" (cq-home-assistant--format-state entity-id payload)))

(defun cq-home-assistant--message-error-callback (error-message)
  "Display Home Assistant ERROR-MESSAGE in the minibuffer."
  (message "%s" error-message))

(defun cq-home-assistant--refresh-success-callback (silent entity-id payload)
  "Update the mode line after a successful async refresh."
  (setq cq-home-assistant--request-in-flight nil)
  (let ((text (cq-home-assistant--format-state entity-id payload)))
    (cq-home-assistant--set-mode-line text)
    (unless silent
      (message "%s" text))))

(defun cq-home-assistant--refresh-error-callback (silent error-message)
  "Handle async Home Assistant refresh errors."
  (setq cq-home-assistant--request-in-flight nil)
  (cq-home-assistant--set-mode-line "HA error")
  (unless silent
    (message "%s" error-message)))

(defun cq-home-assistant-refresh (&optional silent)
  "Refresh the configured Home Assistant entity.
With SILENT non-nil, suppress the minibuffer message."
  (interactive)
  (condition-case err
      (let ((entity-id (cq-home-assistant--current-entity)))
        (unless cq-home-assistant--request-in-flight
          (setq cq-home-assistant--request-in-flight t)
          (cq-home-assistant--request-state-async
           entity-id
           (apply-partially #'cq-home-assistant--refresh-success-callback silent)
           (apply-partially #'cq-home-assistant--refresh-error-callback silent))))
    (error
     (cq-home-assistant--set-mode-line "HA error")
     (unless silent
       (message "%s" (error-message-string err))))))

(defun cq-home-assistant-read-state (entity-id)
  "Fetch and display a Home Assistant state for ENTITY-ID."
  (interactive (list (cq-home-assistant--read-entity-id)))
  (cq-home-assistant--request-state-async
   entity-id
   #'cq-home-assistant--message-state-callback
   #'cq-home-assistant--message-error-callback))

(defun cq-home-assistant-set-default-entity (entity-id)
  "Set the default Home Assistant ENTITY-ID and refresh it."
  (interactive (list (cq-home-assistant--read-entity-id)))
  (setq cq-home-assistant-entity-id entity-id)
  (customize-save-variable 'cq-home-assistant-entity-id entity-id)
  (cq-home-assistant-refresh))

(defun cq-home-assistant-show-default-buffer ()
  "Show the configured Home Assistant entity in a buffer."
  (interactive)
  (cq-home-assistant--request-state-async
   (cq-home-assistant--current-entity)
   #'cq-home-assistant--show-buffer-callback
   #'cq-home-assistant--message-error-callback))

(defun cq-home-assistant--start-timer ()
  "Start the Home Assistant refresh timer."
  (when cq-home-assistant--timer
    (cancel-timer cq-home-assistant--timer))
  (setq cq-home-assistant--timer
        (run-with-timer
         cq-home-assistant-refresh-interval
         cq-home-assistant-refresh-interval
         (lambda ()
           (when cq-home-assistant-mode
             (cq-home-assistant-refresh t))))))

(defun cq-home-assistant--stop-timer ()
  "Stop the Home Assistant refresh timer."
  (when cq-home-assistant--timer
    (cancel-timer cq-home-assistant--timer)
    (setq cq-home-assistant--timer nil)))

;;;###autoload
(define-minor-mode cq-home-assistant-mode
  "Show the configured Home Assistant entity in the global mode line."
  :global t
  :group 'cq-home-assistant
  (if cq-home-assistant-mode
      (progn
        (add-to-list 'global-mode-string cq-home-assistant--mode-line-form t)
        (cq-home-assistant--start-timer)
        (cq-home-assistant-refresh))
    (setq global-mode-string
          (delete cq-home-assistant--mode-line-form global-mode-string))
    (cq-home-assistant--stop-timer)
    (setq cq-home-assistant--mode-line "")
    (force-mode-line-update t)))

(keymap-set cq-home-assistant-prefix-map "b" #'cq-home-assistant-show-default-buffer)
(keymap-set cq-home-assistant-prefix-map "e" #'cq-home-assistant-set-default-entity)
(keymap-set cq-home-assistant-prefix-map "m" #'cq-home-assistant-mode)
(keymap-set cq-home-assistant-prefix-map "r" #'cq-home-assistant-refresh)
(keymap-set cq-home-assistant-prefix-map "s" #'cq-home-assistant-read-state)

(provide 'cq-home-assistant)
;;; cq-home-assistant.el ends here
