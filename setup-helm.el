;;; I prefer helm to ido
(ido-mode nil)

(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'helm-projectile)

(require 'helm-projectile)
(require 'helm-ls-git)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-d") 'helm-projectile)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x c a") 'helm-ag)
(global-set-key (kbd "C-x c o") 'helm-occur)
(global-set-key (kbd "C-c H") 'helm-resume)

(setq helm-split-window-default-side 'right)

(defun helm-magit-status-action (candidate)
  (with-helm-buffer (magit-status candidate)))

(helm-add-action-to-source
 "Magit status"
 'helm-magit-status-action
 helm-source-find-files)

(helm-mode 1)

(provide 'setup-helm)
