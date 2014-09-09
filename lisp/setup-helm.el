;;; I prefer helm to ido
(ido-mode nil)

(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'helm-projectile)

(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-buffers)
(require 'helm-projectile)
(require 'helm-grep)
(require 'helm-ls-git)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-d") 'helm-projectile)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-c H") 'helm-resume)

(global-set-key (kbd "C-c h a") 'helm-ag)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)

(define-key helm-map (kbd "C-d") '(lambda ()
                  (interactive)
                  (helm-select-nth-action 1)))

(setq helm-split-window-default-side 'right)

(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(defun helm-magit-status-action (candidate)
  (with-helm-buffer (magit-status candidate)))

(helm-add-action-to-source
 "Magit status"
 'helm-magit-status-action
 helm-source-find-files)

(helm-mode 1)

(provide 'setup-helm)
