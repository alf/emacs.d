(require-package 'magit)
(global-set-key (kbd "C-x C-z") 'magit-status)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(add-hook 'magit-mode-hook
	  (lambda ()
	    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)))

(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(defun alf/magit-log-folder (dir-name)
  (interactive "DDirectory :")
  (setq magit-refresh-args `(oneline "HEAD" ("--graph" "--" ,dir-name)))
  (magit-refresh))

(unless (fboundp 'magit-gh-pulls-mode)
  (package-install 'magit-gh-pulls))

(eval-after-load 'magit
  '(define-key magit-mode-map "#gg"
     'endless/load-gh-pulls-mode))

(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))

(provide 'setup-magit)
