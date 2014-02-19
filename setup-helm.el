(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'helm-projectile)

(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-d") 'helm-projectile)
(global-set-key (kbd "C-c H") 'helm-resume)

(helm-mode 1)

(provide 'setup-helm)
