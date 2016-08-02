(require 'cl)
(require 'nnir)
(require 'smtpmail-multi)

(defun quark-gnus-auth-sources ()
  "Return the list of all auth sources from the .authinfo[.gpg]
Temporary wrapper around auth-source-search to avoid bug #22188 - solved in emacs25"
  (auth-source-search :port '(25 587 993 465) :max 999))

(defun quark-gnus-preprocess-email (email)
  "Replaces charaters in email address to prepare
it to be interned as a symbol"
  (replace-regexp-in-string (regexp-quote "@") "." email nil 'literal))

(defun quark-gnus-is-smtp (source)
  "Naive way to determine if the source from .authinfo is an
smtp account"
  (let ((host (plist-get source :host))
        (port (plist-get source :port)))
    (or (and host (string-match "^smtp\\." host))
        (and port
             (or
              (string= port "25")
              (string= port "587")
              (string= port "465"))))))

(defun quark-gnus-is-nntp (source)
  "Naive way to determin if source from .authinfo is an nntp
account"
  (let ((host (plist-get source :host))
        (port (plist-get source :port)))
    (or (and host (or (string-match "^news\\." host)
                      (string-match "^nntp\\." host)))
        (and port (or
                   (string= port "119")
                   (string= port "563"))))))

(defun quark-gnus-is-imap (source)
  "Naive way to determin if source from .authinfo is an imap
account"
  (let ((host (plist-get source :host))
        (port (plist-get source :port)))
    (or (and host (or (string-match "^imap\\." host)
                      (string-match "@gmail" host)))
        (and port
             (or
              (string= port "993")
              (string= port "143"))))))

(defun quark-gnus-create-smtpmail-multi-accounts (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo,
create a variable `smtpmail-multi-accounts' from smtpmail-multi
package, assuming login is the email address.
Best suitable for gmail-like services.

If port is not specified use the 25 port and no encryption.
If port is 587 use starttls encryption."
  (let ((accounts nil))
    (dolist (source smtps)
      (let* ((user (plist-get source :user))
             (host (plist-get source :host))
             (port-from-source (plist-get source :port))
             (port (if port-from-source port-from-source "25")))
        (push
         `(,(intern (quark-gnus-preprocess-email user)).
           (,user
            ,host
            ,(string-to-number port)
            ,user
            ,(if (string= port "587") 'starttls 't)
            nil nil nil))
         accounts)))
    `(setq smtp-multi-accounts (quote ,(reverse accounts)))))

(defun quark-gnus-create-smtpmail-multi-associations (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create
a variable `smtpmail-multi-associations' from smtpmail-multi package,
assuming login is the email address."
  (let ((accounts nil))
    (dolist (source smtps)
      (let* ((mail (plist-get source :user))
             (symb (quark-gnus-preprocess-email mail)))
        (push
         `(,mail ,(intern symb))
         accounts)))
    `(setq smtpmail-multi-associations (quote ,(reverse accounts)))))

(defun quark-gnus-create-gnu-posting-styles (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create a variable
`gnus-posting-styles' from GNUS package, assuming
login is the email address.
Best suitable for gmail-like services."
  (let ((accounts nil))
    (dolist (source smtps)
      (let ((mail (plist-get source :user)))
        (push
         `((header "to" ,mail)
           (address ,mail))
         accounts)
        (push
         `((header "cc" ,mail)
           (address ,mail))
         accounts)))
    `(setq gnus-posting-styles (quote ,(reverse accounts)))))

(defun quark-gnus-set-gnus-select-method (nntps)
  "Given the NNTPS - list of NNTP accounts from authinfo, create a variable
`gnus-select-method' from GNUS package, taking the first NNTP account
from the list. If the list is empty, the variable is not changed."
  (when nntps
    (let ((nntp (car nntps)))
      `(setq gnus-select-method '(nntp ,(plist-get nntp :host))))))

(defun quark-gnus-imap-add-to-gnus-secondary-select-methods (imaps is-gmail)
  "Given the IMAPS - list of IMAP accounts from authinfo, append the list
`gnus-secondary-select-methods' from GNUS package with the generated
entries for typical gmail-alike IMAP servers."
  (let ((accounts nil))
    (dolist (source imaps)
      (let ((user (plist-get source :user))
            (host (plist-get source :host)))
        (if (funcall is-gmail source)
            (push
             `(add-to-list 'gnus-secondary-select-methods
                           '(nnimap ,host
                                    (nnimap-address "imap.gmail.com")
                                    (nnimap-server-port "imaps")
                                    (nnimap-stream ssl)
                                    (nnir-search-engine imap)))
             accounts)
          (push
           `(add-to-list 'gnus-secondary-select-methods
                         '(nnimap ,user
                                  (nnimap-address ,host)
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap)))
           accounts))))
    `(progn ,@(reverse accounts))))

(let ((smtps (remove-if-not 'quark-gnus-is-smtp (quark-gnus-auth-sources))))
  (eval (quark-gnus-create-smtpmail-multi-accounts smtps))
  (eval (quark-gnus-create-smtpmail-multi-associations smtps))
  (eval (quark-gnus-create-gnu-posting-styles smtps))
  (setq smtpmail-multi-default-account (caar smtpmail-multi-accounts))
  (setq user-mail-address (second (car smtpmail-multi-accounts))))

(let ((nntps (remove-if-not 'quark-gnus-is-nntp (quark-gnus-auth-sources))))
  (eval (quark-gnus-set-gnus-select-method nntps)))

(let ((imaps (remove-if-not 'quark-gnus-is-imap (quark-gnus-auth-sources))))
  (eval (quark-gnus-imap-add-to-gnus-secondary-select-methods imaps (lambda (h) (string-match "gmail" (plist-get h :host))))))
