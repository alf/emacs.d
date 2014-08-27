(global-set-key (kbd "C-x o")  'other-window)
(global-set-key (kbd "C-x O")  'reverse-other-window)
(global-set-key (kbd "C-c C") 'recompile)
(global-set-key (kbd "M-§") 'other-frame)
(global-set-key (kbd "M-`") 'other-frame)

(defun reverse-other-window ()
  (interactive)
  (other-window -1))

(defun recompile-mvn-debug (&optional edit-command)
  (interactive "P")
  (let ((orig-compile-command compile-command))
    (setq compile-command (concat compile-command " -Dmaven.surefire.debug"))
    (recompile)
    (setq compile-command orig-compile-command)))

(global-set-key (kbd "S-C-c C") 'recompile-mvn-debug)

(defun recompile-mvn-coverage (&optional edit-command)
  (interactive "P")
  (let ((orig-compile-command compile-command))
    (setq compile-command "mvn clean cobertura:cobertura")
    (recompile)
    (setq compile-command orig-compile-command)))

(global-set-key (kbd "M-S-C-c C") 'recompile-mvn-coverage)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

(provide 'setup-keybindings)
