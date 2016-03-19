;; Most of my settings are set using customize
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Add my lisp folder to the load-path so we can require helper
;; functions etc.
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'helpers)

(package-initialize)
(package-refresh-contents)
(package-install-selected-packages)

;; Some OSX specific stuff
(when (eq system-type 'darwin)
  (progn
    ;; I use CAPSLOCK for typing accented characters, this works system
    ;; wide, so Emacs needs to fall in line here.
    (setq ns-alternate-modifier 'none)

    ;; I prefer the command keys for meta, and a symetrical keyboard
    ;; layout so I can alternate which hands holds the control keys
    (setq ns-command-modifier 'meta)

    ;; Include /usr/local/bin in PATH and exec-path
    (alf/add-to-path "/usr/local/bin")))

;; Magit is more important than minimizing emacs
(global-set-key (kbd "C-x C-z") 'magit-status)

;; Use helm because it's awesome
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-p") 'helm-browse-project)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x c s") 'helm-do-grep-ag)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-/") 'helm-dabbrev)

(require 'helm-files)
(define-key helm-find-files-map (kbd "C-s") 'helm-ff-run-grep-ag)

(defun alf/helm-quit-and-magit ()
  "Drop into `magit-status' from `helm'.
If current selection is a file, `magit-status' from its directory."
  (interactive)
  (with-helm-alive-p
    (helm-run-after-exit
     (lambda (f)
       (if (file-exists-p f)
	   (magit-status-internal (file-name-directory f))
	 (magit-status)))
     (helm-get-selection))))

(define-key helm-map (kbd "C-x C-z") 'alf/helm-quit-and-magit)

;; Use helm to browse histories
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode)

;; Use projectile to easily switch between projects
(projectile-global-mode)
