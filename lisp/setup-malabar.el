(require 'cedet)
(require 'semantic)
;(load "semantic/loaddefs.el")
(semantic-mode 1)
(require-package 'malabar-mode)
(require 'malabar-mode)
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(add-hook 'malabar-mode-hook 'subword-mode)

(defun insert-then-trigger ()
  (interactive)
  (insert ".")
  (ac-complete-semantic))

(add-hook 'malabar-mode-hook
	  (lambda ()
	    (define-key malabar-mode-map "." 'insert-then-trigger)))

(require-package 'java-snippets)
(java-snippets-initialize)

(provide 'setup-malabar)
