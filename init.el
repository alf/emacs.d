;;; Make sure emacs can find git, etc.
(push "/usr/local/bin" exec-path)
(push "/Users/alf.lervag/local/bin" exec-path)
(setenv "PATH" (concat (getenv "PATH") ":/Users/alf.lervag/local/bin"))

;;; Be pretty from the start.
(load-theme 'tango-dark)

(setq emacs-dir (file-name-directory
		 (or load-file-name (buffer-file-name))))
(setq dropbox-dir (file-name-as-directory
		   (expand-file-name "../.." emacs-dir)))

(setq custom-file (expand-file-name "custom.el" emacs-dir))

(add-to-list 'load-path (expand-file-name "scripts" emacs-dir))

(if (eq system-type 'darwin) (load "init-mac.el"))

(load "init-package.el")
(load "init-modes.el")
(load "init-org.el")
(load "init-autoloads.el")

;; Do this after loading other stuff so we know we get my configuration.
(load custom-file)
(load "custom-functions.el")
(load "custom-variables.el")

;; This must come after custom-functions since we bind some of them.
(load "init-bindings.el")
