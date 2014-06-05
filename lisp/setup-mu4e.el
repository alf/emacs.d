(defun init--el-get ()
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (let (el-get-master-branch)
        (goto-char (point-max))
        (eval-print-last-sexp)))))


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(init--el-get)
(el-get 'sync '(mu4e))

(setq mu4e-view-show-images t)
(setq mu4e-html2text-command "html2text -utf8 -width 72 -nobs")
(setq mu4e-sent-messages-behavior 'delete)

(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'bbdb-mail-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook
          '(lambda () (setq fill-column 72))
          'append)

(defvar my-mu4e-account-alist
  '(("Bouvet"
     (mu4e-sent-folder "/Bouvet/Sent Items")
     (mu4e-trash-folder "/Bouvet/Deleted Items")
     (mu4e-drafts-folder "/Bouvet/Drafts")
     (user-mail-address "alf.lervag@bouvet.no")
     (message-signature-file ".Bouvet.txt")
     (smtpmail-default-smtp-server "mail.bouvet.no")
     (smtpmail-local-domain "bouvet.no")
     (smtpmail-smtp-server "mail.bouvet.no")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 465))
    ("Lervag"
     (mu4e-sent-folder "/Lervag/Sent Messages")
     (mu4e-trash-folder "/Lervag/Deleted Messages")
     (mu4e-drafts-folder "/Lervag/Drafts")
     (user-mail-address "alf@lervag.net")
     (message-signature-file ".Lervag.txt")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-local-domain "lervag.net")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))))

(defun my-mu4e-set-account (&optional account)
  "Set the account for composing a message."
  (let* ((account
          (or account
              (if mu4e-compose-parent-message
                  (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                    (string-match "/\\(.*?\\)/" maildir)
                    (match-string 1 maildir))
                (completing-read (format "Compose with account: (%s) "
                                         (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                                 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                 nil t nil nil (caar my-mu4e-account-alist)))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(my-mu4e-set-account "Bouvet")
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(setq mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed" "Unread messages" 117)
        ("date:today..now AND NOT flag:trashed" "Today's messages" 116)
        ("date:7d..now AND NOT flag:trashed" "Last 7 days" 119)))

(require 'org-mu4e)

(provide 'setup-mu4e)
