(if (eq system-type 'darwin) (load "init-mac.el"))

;; Setup expansion
(define-key global-map "\M-/" 'dabbrev-expand)
(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'unexpand)

;; quick access to stuff I use a lot
(define-key global-map [(f9)] 'recompile)
(define-key global-map [(f10)] 'matportalen-build)
(define-key global-map [(f11)] 'matportalen-search-templates)

;; upgrade a few builtins
(define-key global-map "\C-x\C-b" 'ibuffer)
(define-key global-map "\C-x\C-d" 'ido-dired)

(add-hook 'ibuffer-mode-hooks '(lambda ()
				 (define-key ibuffer-mode-map [(shift return)] 'ibuffer-visit-buffer-other-window)))


;; Full screen is soooo lovely
(define-key global-map [(meta return)] 'ns-toggle-fullscreen)

;; Make it easier to switch between frames
(define-key global-map "\M-`" 'other-frame)
(define-key global-map "\M-~" 'alf/previous-frame)

;; Make it easier to switch back to previous window
(define-key global-map "\C-xO" 'alf/previous-window)

;; Map the norwegian characters for convenience
(define-key global-map [(super a)]  (kbd "å"))
(define-key global-map [(super A)]  (kbd "Å"))
(define-key global-map [(super o)]  (kbd "ø"))
(define-key global-map [(super O)]  (kbd "Ø"))
(define-key global-map [(super \')] (kbd "æ"))
(define-key global-map [(super \")] (kbd "Æ"))

;; I prefer using meta-t for the textmate-stuff
(add-hook 'textmate-mode-hook 
	  '(lambda ()
	     (add-to-list '*textmate-project-roots* ".bzr")
	     (alf/switch-binding [(super t)] [(meta t)] *textmate-mode-map*)
	     (alf/switch-binding [(super T)] [(meta T)] *textmate-mode-map*)))

;; window movement commands inspired by emacs
(define-key global-map "\C-z" nil)
(define-key global-map "\C-zh" 'windmove-left)
(define-key global-map "\C-zl" 'windmove-right)
(define-key global-map "\C-zk" 'windmove-up)
(define-key global-map "\C-zj" 'windmove-down)

