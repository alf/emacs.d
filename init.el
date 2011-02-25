(setq dotfiles-dir (file-name-directory
                    (or load-file-name (buffer-file-name))))

(let ((default-directory dotfiles-dir))
  (setq package-user-dir
	(expand-file-name "elpa"))
  (setq custom-file
	(expand-file-name "custom.el"))

  (load custom-file)

  (if (eq system-type 'darwin)
      (load
       (expand-file-name "init-mac.el"))))

(push "/usr/local/bin" exec-path)
(load-theme 'tango)
