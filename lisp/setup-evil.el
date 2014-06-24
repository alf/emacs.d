(require-package 'evil)
(require 'evil)
(evil-mode 1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-SPC") 'evil-normal-state)

(add-hook 'org-capture-mode-hook 'evil-emacs-state)
(add-hook 'org-capture-prepare-finalize-hook 'evil-normal-state)
(evil-set-initial-state 'magit-process-mode 'emacs)
(evil-set-initial-state 'jira-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'git-commit-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
    (lambda ()
      (let ((color (cond ((minibufferp) default-color)
                         ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                         ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                         ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                         (t default-color))))
        (set-face-background 'mode-line (car color))
        (set-face-foreground 'mode-line (cdr color))))))

(provide 'setup-evil)
