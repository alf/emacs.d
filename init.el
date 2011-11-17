;;; Make sure emacs can find my homebrew installed stuff
(push "/usr/local/bin" exec-path)
(push "/usr/local/Cellar/python/2.7.1/bin" exec-path)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/local/Cellar/python/2.7.1/bin"))

(setq user-emacs-directory (file-name-directory
		 (or load-file-name (buffer-file-name))))
(setq custom-theme-directory (file-name-as-directory
		   (expand-file-name "themes" user-emacs-directory)))
(setq dropbox-dir (file-name-as-directory
		   (expand-file-name "../.." user-emacs-directory)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/django-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/zencoding" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/html5-el" user-emacs-directory))

(if (eq system-type 'darwin) (load "init-mac.el"))

(load "custom-functions.el")
(load "init-bindings.el") ;; depends on custom-functions.el

(load "init-package.el")
(load "init-modes.el")
(load "init-hooks.el")
(load "init-org.el")
(load "init-autoloads.el")

;; TODO Find out where the code in here should be.  Currently it's
;; just maven stuff
(load "custom-variables.el")


