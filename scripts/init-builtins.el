(ido-mode t)
(setq ido-enable-flex-matching t)
(server-mode t)
(winner-mode t)
(require 'dired-x)
(hl-needed-mode t)
(setq hl-needed-on-config-change nil)

(define-key global-map "\M-/" 'dabbrev-expand)
(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'unexpand)
(define-key global-map [(f9)] 'recompile)
(define-key global-map "\C-x\C-b" 'ibuffer)

(savehist-mode t)
(desktop-save-mode t)

(setq dabbrev-case-fold-search 1)

(define-key global-map "\M-" 'ns-toggle-fullscreen)

(setq auto-mode-alist
	(cons '("\\.zcml\\'" . nxml-mode)
		auto-mode-alist))

(setq auto-mode-alist
	(cons '("\\.pt\\'" . nxml-mode)
		auto-mode-alist))
