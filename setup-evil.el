(require-package 'evil)
(require 'evil)
(evil-mode 1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-SPC") 'evil-normal-state)

;; Doesn't seem to work, but we'll keep it for now so I know that I've tried it.
;(evil-set-initial-state 'org-capture-mode 'insert)
(evil-set-initial-state 'magit-process-mode 'emacs)
;(add-hook 'org-capture-mode-hook 'evil-append-line)
(provide 'setup-evil)
