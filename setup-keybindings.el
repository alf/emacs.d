(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)
(global-set-key (kbd "C-x o")  'next-multiframe-window)
(global-set-key (kbd "C-x O")  'previous-multiframe-window)
(global-set-key (kbd "<f1>") 'recompile)
(global-set-key (kbd "M-§") 'other-frame)

(defun recompile-mvn-debug (&optional edit-command)
  (interactive "P")
  (let ((orig-compile-command compile-command))
    (setq compile-command (concat compile-command " -Dmaven.surefire.debug"))
    (recompile)
    (setq compile-command orig-compile-command)))
  
(global-set-key (kbd "S-<f1>") 'recompile-mvn-debug)

(defun recompile-mvn-coverage (&optional edit-command)
  (interactive "P")
  (let ((orig-compile-command compile-command))
    (setq compile-command "mvn clean cobertura:cobertura")
    (recompile)
    (setq compile-command orig-compile-command)))

(global-set-key (kbd "M-S-<f1>") 'recompile-mvn-coverage)

(global-set-key "\M-/" 'auto-complete)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

(provide 'setup-keybindings)
