(require 'helm-config)
(require 'helm-ls-git)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(helm-mode 1)

(provide 'setup-helm)
