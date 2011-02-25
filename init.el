;;; Make sure emacs can find git, etc.
(push "/usr/local/bin" exec-path)

;;; Be pretty from the start.
(load-theme 'tango)

(setq emacs-dir (file-name-directory
                    (or load-file-name (buffer-file-name))))
(setq dropbox-dir (expand-file-name "../.." emacs-dir))

(setq custom-file (expand-file-name "custom.el" emacs-dir))
(load custom-file)

(add-to-list 'load-path (expand-file-name "scripts" emacs-dir))

(if (eq system-type 'darwin) (load "init-mac.el"))

(load "init-elpa.el")
(load "init-packages.el")
(load "init-org.el")

