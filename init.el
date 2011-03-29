;;; Make sure emacs can find my homebrew installed stuff
(push "/usr/local/bin" exec-path)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

;;; Be pretty from the start.
(load-theme 'tango-dark)

(setq user-emacs-directory (file-name-directory
		 (or load-file-name (buffer-file-name))))
(setq dropbox-dir (file-name-as-directory
		   (expand-file-name "../.." user-emacs-directory)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
(if (eq system-type 'darwin) (load "init-mac.el"))

(load "init-package.el")
(load "init-modes.el")
(load "init-org.el")
(load "init-autoloads.el")

;; Do this after loading other stuff so we know we get my configuration.
(load "custom-functions.el")
(load "custom-variables.el")

;; This must come after custom-functions since we bind some of them.
(load "init-bindings.el")
