(require 'auto-complete-config)
(ac-config-default)
(set-default 'ac-sources
             '(ac-source-abbrev
               ac-source-dictionary
               ac-source-yasnippet
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))
(global-auto-complete-mode t)
(provide 'setup-auto-complete)
