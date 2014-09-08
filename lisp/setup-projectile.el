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

(define-key helm-map (kbd "C-d") '(lambda ()
                  (interactive)
                  (helm-select-nth-action 1)))

(provide 'setup-projectile)
