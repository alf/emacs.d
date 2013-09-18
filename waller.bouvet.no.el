(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials "~/.authinfo"
      smtpmail-smtp-user "alf.lervag@bouvet.no"
      smtpmail-default-smtp-server "mail.bouvet.no"
      smtpmail-smtp-server "mail.bouvet.no"
      smtpmail-smtp-service 465
      smtpmail-sendto-domain "bouvet.no"
      smtpmail-local-domain "bouvet.no")

(require 'mime-w3m)
(setq wl-disable-auto-save t)
(setq elmo-imap4-force-login t)
(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(setq elmo-imap4-default-server "mail.bouvet.no"
      elmo-imap4-default-user "alf.lervag@bouvet.no"
      elmo-imap4-default-authenticate-type 'auth
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-default-port 993)

(setq smtp-default-server "mail.bouvet.no"
      smtp-service 587)

(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "login"
      wl-smtp-posting-server "mail.bouvet.no"
      wl-smtp-posting-user "alf.lervag@bouvet.no"
      wl-from "alf.lervag@bouvet.no"
      wl-local-domain "bouvet.no"
      wl-message-id-domain "mail.bouvet.no")
