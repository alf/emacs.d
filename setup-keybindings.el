(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)
(global-set-key (kbd "C-x o")  'next-multiframe-window)
(global-set-key (kbd "C-x O")  'previous-multiframe-window)
(global-set-key (kbd "<f1>") 'recompile)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

(provide 'setup-keybindings)
