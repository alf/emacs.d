;; Setup expansion
;(define-key global-map "\M-/" 'dabbrev-expand)
;(define-key read-expression-map [(tab)] 'hippie-expand)
;(define-key read-expression-map [(shift tab)] 'unexpand)

(if (eq system-type 'darwin) (load "init-mac.el"))

;; quick access to stuff I use a lot
(define-key global-map [(f9)] 'recompile)
(define-key global-map [(f10)] 'matportalen-build)
(define-key global-map [(f11)] 'matportalen-search-templates)

;; upgrade a few builtins
(define-key global-map "\C-x\C-b" 'ibuffer)
(define-key global-map "\C-x\C-d" 'ido-dired)

(add-hook 'ibuffer-mode-hooks '(lambda ()
				 (define-key ibuffer-mode-map [(shift return)] 'ibuffer-visit-buffer-other-window)))

(defun alf/textmate-mode-hook ()
  (add-to-list '*textmate-project-roots* ".bzr")
  )

(add-hook 'textmate-mode-hook 'alf/textmate-mode-hook)



;; Full screen is soooo lovely
(define-key global-map [(ctrl meta f)] 'ns-toggle-fullscreen)

;; Make it easier to switch between frames
(define-key global-map "\M-`" 'other-frame)
(define-key global-map "\M-~" 'alf/previous-frame)

;; Make it easier to switch back to previous window
(define-key global-map "\C-xO" 'alf/previous-window)

;; window movement commands inspired by emacs
(define-key global-map "\C-z" nil)
(define-key global-map "\C-zh" 'windmove-left)
(define-key global-map "\C-zl" 'windmove-right)
(define-key global-map "\C-zk" 'windmove-up)
(define-key global-map "\C-zj" 'windmove-down)
