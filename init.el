(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path user-emacs-directory)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(require 'setup-package)

(defun init--install-packages ()
  (packages-install
   '(org
     ack
     magit
     gist
     htmlize
     visual-regexp
     flycheck
     flx
     flx-ido
     css-eldoc
     yasnippet
     smartparens
     ido-vertical-mode
     ido-at-point
     simple-httpd
     guide-key
     restclient
     highlight-escape-sequences
     whitespace-cleanup-mode
     elisp-slime-nav
     git-commit-mode
     gitconfig-mode
     gitignore-mode
     clojure-mode
     cider
     cider-tracing)))

(init--install-packages)

(require 'setup-org-mode)
(require 'setup-magit)
(autoload 'skewer-start "setup-skewer" nil t)
(autoload 'skewer-demo "setup-skewer" nil t)

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require-package 'move-text)
(move-text-default-bindings)

(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(eval-after-load 'eshell '(require 'setup-eshell))
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'setup-mu4e)
