(when (require 'session nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring))

(add-hook 'after-init-hook 'session-initialize)

(provide 'setup-session)
