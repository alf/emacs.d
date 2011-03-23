(define-key global-map "\M-/" 'dabbrev-expand)
(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'unexpand)
(define-key global-map [(f9)] 'recompile)
(define-key global-map "\C-x\C-b" 'ibuffer)
(define-key global-map "\M-" 'ns-toggle-fullscreen)
