(defun alf/eshell-here ()
  (interactive)
  (kill-new (file-name-directory buffer-file-name))
  (switch-to-buffer "*eshell*")
  (insert "cd ")
  (yank))

(provide 'setup-eshell)
