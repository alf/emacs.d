(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path user-emacs-directory)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(require 'setup-package)

(defun init--install-packages ()
  (packages-install
   '(org
     magit
     paredit
     move-text
     god-mode
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
