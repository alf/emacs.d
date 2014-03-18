(setenv "PATH" (concat "/opt/boxen/homebrew/bin:" (getenv "PATH")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path user-emacs-directory)

(require 'setup-package)
(require 'setup-better-defaults)
(require 'setup-session)
(load custom-file)

(defun init--install-packages ()
  (packages-install
   '(ack
     gist
     htmlize
     visual-regexp
     flycheck
     css-eldoc
     yasnippet
     simple-httpd
     restclient
     highlight-escape-sequences
     whitespace-cleanup-mode
     elisp-slime-nav
     git-commit-mode
     gitconfig-mode
     gitignore-mode
     clojure-mode
     sparql-mode
     csharp-mode
     cider
     cider-tracing)))

(init--install-packages)

(require 'setup-key-chord)
(require 'setup-org-mode)
(require 'setup-org-jira)
(require 'setup-magit)
(require 'setup-skewer)
(require 'setup-expand-region)
(require 'setup-ace-jump-mode)
(require 'setup-move-text)
(require 'setup-exec-path-from-shell)
(require 'setup-eshell)
(require 'setup-mu4e)
(require 'setup-smartparens)
(require 'setup-keybindings)
(require 'setup-projectile)
(require 'setup-goto-last-change)
(require 'setup-auto-complete)
(require 'setup-malabar)
(require 'setup-jad)
(require 'setup-erc)
(require 'custom-functions)
(require 'setup-helm)
(require 'setup-guide-key)
(require 'setup-evil)
(require 'setup-flyspell)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
