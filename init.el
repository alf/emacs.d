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
     exec-path-from-shell
     expand-region
     magit
     paredit
     move-text
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

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'move-text)
(move-text-default-bindings)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(eval-after-load 'eshell '(require 'setup-eshell))
