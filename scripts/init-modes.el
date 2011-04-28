(setq auto-mode-alist
	(cons '("\\.zcml\\'" . nxml-mode)
		auto-mode-alist))

(setq auto-mode-alist
	(cons '("\\.pt\\'" . nxml-mode)
		auto-mode-alist))

(add-hook 'python-mode-hook
	  (lambda()
	    (set (make-local-variable 'compile-command) (concat "python " (buffer-name)))))
