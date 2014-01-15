(defun decompile-region ()
  (interactive)
  (let ((command "/usr/local/bin/jad -space -t2 -p /dev/stdin"))
    (shell-command-on-region (region-beginning) (region-end) command (current-buffer) t)))

(defun decompile-buffer ()
  (interactive)
  (beginning-of-buffer)
  (set-mark (point-max))
  (decompile-region))

(provide 'jad)
