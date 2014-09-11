;;; Global key bindings
(define-key ctl-x-map "o" 'other-window)
(define-key ctl-x-map "O" 'reverse-other-window)

(global-set-key (kbd "C-c C") 'recompile)

(global-set-key (kbd "M-§") 'other-frame)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

;;; Toggle features easily with: C-x t <key>
;;; From http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(define-prefix-command 'alf/toggle-map)
(define-key ctl-x-map "t" 'alf/toggle-map)
(define-key alf/toggle-map "c" 'column-number-mode)
(define-key alf/toggle-map "d" 'toggle-debug-on-error)
(define-key alf/toggle-map "e" 'toggle-debug-on-error)
(define-key alf/toggle-map "f" 'auto-fill-mode)
(define-key alf/toggle-map "l" 'toggle-truncate-lines)
(define-key alf/toggle-map "q" 'toggle-debug-on-quit)
(define-key alf/toggle-map "n" #'narrow-or-widen-dwim)

;;; "C-x t n" to enter org src blocks, and "C-x C-s" to exit
(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

;;; Some convenient bindings when working with java
(global-set-key (kbd "S-C-c C") 'recompile-mvn-debug)
(global-set-key (kbd "M-S-C-c C") 'recompile-mvn-coverage)

;;; My very own prefix key
(define-prefix-command 'alf/ctl-z-map)
(define-key global-map "\C-z" 'alf/ctl-z-map)

(provide 'setup-keybindings)
