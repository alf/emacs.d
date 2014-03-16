(require-package 'better-defaults)

;; I don't have time to write yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)


(provide 'setup-better-defaults)
