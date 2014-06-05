(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'setup-exec-path-from-shell)
