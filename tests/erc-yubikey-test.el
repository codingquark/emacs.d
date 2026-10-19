;;; erc-yubikey-test.el --- Offline checks for the Libera connector -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'use-package)
(require 'erc)

;; Load only the actual IRC block, without the host's private settings.
(defconst cq-erc-test--config-form
  (let ((source (expand-file-name "../config.org"
                                  (file-name-directory load-file-name))))
    (with-temp-buffer
      (insert-file-contents source)
      (goto-char (point-min))
      (re-search-forward "^\\* IRC$")
      (re-search-forward "^#\\+BEGIN_SRC emacs-lisp$")
      (read (current-buffer)))))

(let ((user-emacs-directory
       (file-name-as-directory (make-temp-file "erc-config-test-" t))))
  (unwind-protect
      (eval cq-erc-test--config-form t)
    (delete-directory user-emacs-directory t)))

(ert-deftest cq-erc-libera-missing-private-settings-before-connect ()
  (dolist (key-uri '(nil "" "/software-key.pem"))
    (let ((cq-erc-libera-key-uri key-uri))
      (cl-letf (((symbol-function 'erc-tls)
                 (lambda (&rest _) (ert-fail "Unexpected connection")))
                ((symbol-function 'read-passwd)
                 (lambda (&rest _) (ert-fail "Unexpected PIN prompt"))))
        (should-error (cq-erc-libera) :type 'user-error)))))

(ert-deftest cq-erc-libera-loads-private-settings ()
  (let* ((user-emacs-directory
          (file-name-as-directory (make-temp-file "erc-private-test-" t)))
         (cq-erc-libera-key-uri nil)
         (cq-erc-libera-certificate nil)
         captured)
    (unwind-protect
        (progn
          (make-directory (expand-file-name "private/erc" user-emacs-directory) t)
          (with-temp-file (expand-file-name "private/erc/config.el"
                                           user-emacs-directory)
            (insert "(setq cq-erc-libera-key-uri \"pkcs11:serial=TEST-TOKEN;id=%01;type=private\"\n"
                    "      cq-erc-libera-certificate \"fixture-cert.pem\")\n"))
          (eval cq-erc-test--config-form t)
          (cl-letf (((symbol-function 'erc-tls)
                     (lambda (&rest args) (setq captured args))))
            (cq-erc-libera "test-nick"))
          (should (equal (plist-get captured :client-certificate)
                         '("pkcs11:serial=TEST-TOKEN;id=%01;type=private"
                           "fixture-cert.pem")))
          (should (equal (plist-get captured :nick) "test-nick")))
      (delete-directory user-emacs-directory t))))

(ert-deftest cq-erc-libera-rejects-another-endpoint-before-pin ()
  (cl-letf (((symbol-function 'read-passwd)
             (lambda (&rest _) (ert-fail "Unexpected PIN prompt"))))
    (should-error (cq-erc-libera-tls "test" nil "example.com" 6697))
    (should-error (cq-erc-libera-tls "test" nil "irc.libera.chat" 6667))))

(ert-deftest cq-erc-libera-missing-certificate-before-pin ()
  (cl-letf (((symbol-function 'file-readable-p) (lambda (_) nil))
            ((symbol-function 'read-passwd)
             (lambda (&rest _) (ert-fail "Unexpected PIN prompt"))))
    (should-error (cq-erc-libera-tls "test" nil "irc.libera.chat" 6697
                                    :client-certificate '("pkcs11:" "missing"))
                  :type 'user-error)))

(ert-deftest cq-erc-libera-clears-pin-after-success ()
  (let ((pin (copy-sequence "test-pin")) captured deleted)
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) pin))
              ((symbol-function 'file-readable-p) (lambda (_) t))
              ((symbol-function 'make-network-process)
               (lambda (&rest args) (setq captured args) 'fake-process))
              ((symbol-function 'process-status) (lambda (_) 'open))
              ((symbol-function 'delete-process) (lambda (_) (setq deleted t))))
      (should (eq 'fake-process
                  (cq-erc-libera-tls "test" nil "irc.libera.chat" 6697
                                     :client-certificate '("pkcs11:test" "cert"))))
      (let ((tls (cdr (plist-get captured :tls-parameters))))
        (should (eq t (plist-get tls :verify-error)))
        (should (equal "irc.libera.chat" (plist-get tls :hostname)))
        (should (equal '(("pkcs11:test" "cert")) (plist-get tls :keylist)))
        (should-not (plist-get tls :pass)))
      (should (equal pin (make-string (length pin) 0)))
      (should-not deleted))))

(ert-deftest cq-erc-libera-tls-failure-clears-pin-and-closes ()
  (let ((pin (copy-sequence "test-pin")) deleted)
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) pin))
              ((symbol-function 'file-readable-p) (lambda (_) t))
              ((symbol-function 'make-network-process) (lambda (&rest _) 'failed))
              ((symbol-function 'process-status) (lambda (_) 'failed))
              ((symbol-function 'delete-process) (lambda (p) (setq deleted p))))
      (should-error (cq-erc-libera-tls "test" nil "irc.libera.chat" 6697
                                      :client-certificate '("pkcs11:" "cert"))
                    :type 'user-error)
      (should (eq deleted 'failed))
      (should (equal pin (make-string (length pin) 0))))))

(ert-deftest cq-erc-libera-cancel-clears-pin-and-closes ()
  (let ((pin (copy-sequence "test-pin")) deleted cancelled)
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) pin))
              ((symbol-function 'file-readable-p) (lambda (_) t))
              ((symbol-function 'make-network-process) (lambda (&rest _) 'pending))
              ((symbol-function 'process-status) (lambda (_) 'connect))
              ((symbol-function 'accept-process-output) (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'delete-process) (lambda (p) (setq deleted p))))
      (condition-case nil
          (cq-erc-libera-tls "test" nil "irc.libera.chat" 6697
                             :client-certificate '("pkcs11:" "cert"))
        (quit (setq cancelled t)))
      (should cancelled)
      (should (eq deleted 'pending))
      (should (equal pin (make-string (length pin) 0))))))

(ert-deftest cq-erc-libera-rejects-registration-without-sasl ()
  (with-temp-buffer
    (setq-local erc-session-connector #'cq-erc-libera-tls)
    (cq-erc-libera-session-setup)
    (should-not erc-server-auto-reconnect)
    (should-not erc-auth-source-server-function)
    (should-not erc-use-auth-source-for-nickserv-password)
    (should-not erc-prompt-for-nickserv-password)
    (should-not erc-nickserv-passwords)
    (let (deleted welcomed)
      (cl-letf (((symbol-function 'delete-process) (lambda (p) (setq deleted p)))
                ((symbol-function 'erc-server-001) (lambda (&rest _) (setq welcomed t))))
        (should (run-hook-with-args-until-success
                 'erc-server-001-functions 'unauthenticated nil))
        (should (eq deleted 'unauthenticated))
        (should-not welcomed)))))

(ert-run-tests-batch-and-exit)
