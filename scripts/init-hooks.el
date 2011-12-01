(require 'paredit)

(add-hook 'dired-load-hook (lambda () (load "dired-x")))

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook     'enable-paredit-mode)
(add-hook 'clojure-mode-hook    'enable-paredit-mode)
