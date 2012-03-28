(setq gnus-select-method '(nnimap "exchange"
                                  (nnimap-stream network)
                                  (nnimap-address "localhost")
                                  (nnimap-server-port 1143)))

(custom-set-variables
 '(send-mail-function 'smtpmail-send-it))
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials "~/.authinfo"
      smtpmail-smtp-user "alf.lervag@bouvet.no"
      smtpmail-default-smtp-server "localhost"
      smtpmail-smtp-server "localhost"
      smtpmail-smtp-service 1025
      smtpmail-sendto-domain "bouvet.no"
      smtpmail-local-domain "bouvet.no")
