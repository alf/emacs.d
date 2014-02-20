(require-package 'guide-key)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/guide-key-sequence '("C-x a" "C-x c" "C-x r" "C-c" "C-x"))
(guide-key-mode 1)

(provide 'setup-guide-key)
