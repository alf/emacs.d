(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Problem with TRAMP mode
;; Control Path too long error
;; TMPDIR variable is really large
;; http://lists.macosforge.org/pipermail/macports-tickets/2011-June/084295.html
(setenv "TMPDIR" "/tmp")

(provide 'setup-environment)
