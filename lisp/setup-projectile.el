(require-package 'projectile)

(setq projectile-completion-system 'helm)
(projectile-global-mode)

;; Prefer this one since it gives me helm actions
(define-key projectile-mode-map (kbd "C-c p p") 'helm-projectile-switch-project)

;; This lets me use fig run to run tests in projectile
(add-to-list 'projectile-project-root-files-bottom-up "fig.yml")

(provide 'setup-projectile)
