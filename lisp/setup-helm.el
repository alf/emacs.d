;;; I prefer helm to ido
(ido-mode nil)

(require-package 'helm)
(require-package 'helm-ag)
(require-package 'helm-projectile)

(setq helm-command-prefix-key "C-c h")

(require 'helm-files)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-buffers)
(require 'helm-projectile)
(require 'helm-grep)

(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-d") 'helm-projectile)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-c H") 'helm-resume)

(global-set-key (kbd "C-c h a") 'helm-ag)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-c h y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)

;; Use helm to browse shell history
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; Use helm to browse minibuffer history
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)


(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

(define-key helm-map (kbd "C-d") '(lambda ()
                  (interactive)
                  (helm-select-nth-action 1)))

(setq helm-split-window-default-side 'right)

(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode 1)

(provide 'setup-helm)
