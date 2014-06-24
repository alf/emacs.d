;; HipChat

(setq jabber-account-list '(("93938_685034@chat.hipchat.com")))
(defvar hipchat-number "93938")
(defvar hipchat-nickname "Alf Lervåg")

(defun hipchat-join-sesam ()
  (interactive)
  (hipchat-join "bouvet_norge_as"))

;; Join a room
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (jabber-read-account)
   (concat hipchat-number "_" room "@conf.hipchat.com")
   hipchat-nickname
   t))

;; Mention nicknames in a way that HipChat clients will pickup
(defun hipchat-mention (nickname)
  (interactive
    (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
      (insert (concat "@\"" nickname "\" ")))

(provide 'setup-hipchat)
