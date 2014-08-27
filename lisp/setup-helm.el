;;; I prefer helm to ido
(ido-mode nil)

(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'helm-projectile)

(require 'helm-buffers)
(require 'helm-projectile)
(require 'helm-ls-git)
(require 'helm-config)

(defun helm-prelude ()
  "Preconfigured `helm'."
  (interactive)
  (condition-case nil
      (if (projectile-project-root)
          (helm-projectile)
        ;; otherwise fallback to `helm-mini'
        (helm-mini))
    ;; fall back to helm mini if an error occurs (usually in `projectile-project-root')
    (error (helm-mini))))

;; History of compile commands.
(defvar alf/rebuffer-history nil)
(defvar alf/rebuffer-input nil
  "input to use for rebuffer")

(defun alf/helm-rebuffer (&optional prefill)
  (interactive "P")
  (when (or prefill (not alf/rebuffer-input))
    (setq alf/rebuffer-input (read-string "Input: " nil 'alf/rebuffer-history)))

  (helm :sources '(helm-c-source-buffers-list helm-c-source-buffer-not-found)
        :buffer "*helm-java-buffers*"
        :input alf/rebuffer-input))

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c h") 'helm-prelude)
(global-set-key (kbd "C-c j") 'alf/helm-rebuffer)
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
