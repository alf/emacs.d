(require-package 'projectile)

(setq projectile-completion-system 'helm)
(projectile-global-mode)

;; Prefer this one since it gives me helm actions

(defun alf/helm-projectile-find-files ()
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources '(helm-source-projectile-files-list
                     helm-source-projectile-buffers-list
                     helm-source-projectile-recentf-list)
          :buffer "*helm projectile*"
          :prompt (projectile-prepend-project-name "pattern: "))))

(define-key projectile-mode-map (kbd "C-c p p") 'helm-projectile-switch-project)
(setq projectile-switch-project-action 'alf/helm-projectile-find-files)

;; This lets me use fig run to run tests in projectile
(add-to-list 'projectile-project-root-files-bottom-up "fig.yml")

(provide 'setup-projectile)
