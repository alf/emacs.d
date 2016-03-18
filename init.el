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
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "M-/") 'helm-dabbrev)

;; Use projectile to easily switch between projects
(projectile-global-mode)
