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

;; Full screen is soooo lovely
(define-key global-map "\M-" 'ns-toggle-fullscreen)

;; Make it easier to switch between frames
(define-key global-map "\M-`" 'other-frame)
(define-key global-map "\M-~" 'alf/previous-frame)

;; Make it easier to switch back to previous window
(define-key global-map "\C-xO" 'alf/previous-window)
