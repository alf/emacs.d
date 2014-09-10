(require-package 'ace-jump-mode)
(global-set-key (kbd "C-x SPC")  'ace-jump-mode)

(key-chord-define-global "xb"     'ace-jump-mode)

(provide 'setup-ace-jump-mode)
