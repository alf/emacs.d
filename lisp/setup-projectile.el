(require-package 'projectile)
(require-package 'ggtags)

(setq projectile-completion-system 'helm)
(projectile-global-mode)

(defun alf/helm-projectile-find-files ()
  "Prefer magit if the project is in git, otherwise use dired"
  (if (file-exists-p ".git")
      (magit-status default-directory)
    (dired default-directory)))

(define-key projectile-mode-map (kbd "C-c p p") 'helm-projectile-switch-project)
(setq projectile-switch-project-action 'alf/helm-projectile-find-files)

(provide 'setup-projectile)
