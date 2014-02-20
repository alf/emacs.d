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
(global-set-key (kbd "C-c H") 'helm-resume)

(helm-add-action-to-source
 "Magit status"
 #'(lambda (_candidate) (with-helm-buffer (magit-status helm-default-directory)))
 helm-source-find-files)

(helm-mode 1)

(provide 'setup-helm)
