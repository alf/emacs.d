(require-package 'graphviz-dot-mode)
(require 'graphviz-dot-mode)

(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
(provide 'setup-graphviz)
