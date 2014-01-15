(require 'smartparens-config)

(sp-use-paredit-bindings)
(add-hook 'malabar-mode-hook 'smartparens-mode)

;; Function relating to customizing something relating to smartparens
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun customize-smartparens ()
  "Customizes a part of smartparens.  Adds auto-expansion of {|} to {\n|\n} on press of enter with | being cursor."
  (sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'shell-script-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'vala-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'perl-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'java-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'sh-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'csharp-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

(customize-smartparens)

(provide 'setup-smartparens)
