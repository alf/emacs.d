;;; helpers.el --- -*- lexical-binding: t -*-

(defun alf/add-to-path (path)
  "Add given path to exec-path and the PATH environment variable"
  (interactive)
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat path path-separator (getenv "PATH"))))

(provide 'helpers)
