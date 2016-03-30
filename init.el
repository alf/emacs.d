(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

;; Most of my settings are set using customize
(load (expand-file-name "custom.el" user-emacs-directory))

;; Set up elpa now so we can found the modules we're using.
(package-initialize)

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)

;; Some OSX specific stuff
(when (eq system-type 'darwin)
  ;; I use CAPSLOCK for typing accented characters, this works system
  ;; wide, so Emacs needs to fall in line here.
  (setq ns-alternate-modifier 'none)

  ;; I prefer the command keys for meta, and a symetrical keyboard
  ;; layout so I can alternate which hands holds the control keys
  (setq ns-command-modifier 'meta)

  ;; Make sure we can find the programs we need.
  (use-package exec-path-from-shell
    :demand t
    :config
    (exec-path-from-shell-initialize)))

;; Some Windows specific stuff
(when (eq system-type 'windows-nt)
  ;; Make $HOME the default directory
  (cd "~"))

(use-package magit
  :demand t
  :bind (("C-x C-z" . magit-status)))

(use-package helm
  :demand t
  :preface
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
  :commands helm-eshell-history
  :bind (("C-h"     . helm-command-prefix)
	 ("M-y"     . helm-show-kill-ring)
	 ("M-/"     . helm-dabbrev)
         ("C-x f"   . helm-multi-files)
         ("C-h P"   . helm-list-emacs-process)
         ("C-h k"   . describe-key)
         ("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-mini)
         ("M-H"     . helm-resume)
	 :map helm-map
	 ("C-x C-z" . alf/helm-quit-and-magit)
	 :map helm-find-files-map
	 ("C-s" . helm-ff-run-grep-ag)
	 ;; Use helm to browse histories
	 :map shell-mode-map
	 ("C-c C-l" . helm-comint-input-ring)
	 :map minibuffer-local-map
	 ("C-c C-l" . helm-minibuffer-history))

  :config
  (require 'helm-config)
  (require 'helm-files)
  (helm-mode))

(use-package helm-descbinds
  :bind (("C-h b"   . helm-descbinds)))

(use-package eshell
  :preface
  (defun eshell-initialize ()
    (bind-key "C-c C-l" 'helm-eshell-history eshell-mode-map))
  :config
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize))

(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))

;; Use projectile to easily switch between projects
(use-package projectile
  :demand t
  :config
  (use-package helm-projectile
    :bind (("C-h p"   . helm-projectile)))
  (projectile-global-mode))

;;; Toggle features easily with: C-x t <key>
;;; From http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(bind-keys :prefix-map alf/toggle-map
           :prefix "C-x t"
           ("c"  . column-number-mode)
           ("e"  . toggle-debug-on-error)
           ("f"  . auto-fill-mode)
           ("t"  . toggle-truncate-lines)
           ("q"  . toggle-debug-on-quit)
           ("" . toggle-frame-fullscreen))

(use-package ace-jump-mode
  :defer t
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)
  (bind-key "C-;" 'ace-jump-mode))

(use-package csharp-mode
  :mode "\\.rb\\'")

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

;; Load my org-mode settings, this is pretty big so I've moved it to a different file.
(load "~/.emacs.d/init-org.el")
