;;; -*- lexical-binding: t -*-

(require 'package)

(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(smex magit)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(let ((alf-system-config (concat user-emacs-directory system-name ".el")))
  (when (file-exists-p alf-system-config)
    (load alf-system-config)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/django-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/zencoding" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/html5-el" user-emacs-directory))

(load "custom-functions.el")
(load "init-bindings.el") ;; depends on custom-functions.el

(load "init-modes.el")
(load "init-hooks.el")
(load "init-org.el")
(load "init-autoloads.el")

;; TODO Find out where the code in here should be.  Currently it's
;; just maven stuff
(load "custom-variables.el")


