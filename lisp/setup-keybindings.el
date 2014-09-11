(global-set-key (kbd "C-x o")  'other-window)
(global-set-key (kbd "C-x O")  'reverse-other-window)
(global-set-key (kbd "C-c C") 'recompile)
(global-set-key (kbd "M-§") 'other-frame)
(global-set-key (kbd "M-`") 'other-frame)



(global-set-key (kbd "S-C-c C") 'recompile-mvn-debug)
(global-set-key (kbd "M-S-C-c C") 'recompile-mvn-coverage)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

(provide 'setup-keybindings)
