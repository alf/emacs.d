(require 'cedet)
(require 'semantic)
;(load "semantic/loaddefs.el")
(semantic-mode 1)

(require 'malabar-mode)

(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

(defun insert-then-trigger ()
  (interactive)
  (insert ".")
  (ac-complete-semantic))

(define-key malabar-mode-map "." 'insert-then-trigger)
(provide 'setup-malabar)
