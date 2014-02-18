(setenv "PATH" (concat "/opt/boxen/homebrew/bin:" (getenv "PATH")))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path user-emacs-directory)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(require 'setup-package)

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
     guide-key
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
     cider-tracing
     dot-mode
     session
     auto-complete)))

(init--install-packages)

(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

(require 'setup-org-mode)
(require 'setup-magit)
(require 'setup-skewer)
(require 'setup-expand-region)
(require 'setup-ace-jump-mode)
(require 'setup-move-text)
(require 'setup-exec-path-from-shell)
(eval-after-load 'eshell '(require 'setup-eshell))
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'setup-mu4e)
(require 'setup-smartparens)
(require 'setup-keybindings)
(require 'setup-projectile)
(require 'setup-goto-last-change)
(require 'setup-auto-complete)
(require 'setup-malabar)
(require 'setup-jad)

(when (require 'session nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring))

(add-hook 'after-init-hook 'session-initialize)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(require 'custom-functions)
(require 'setup-helm)

(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/guide-key-sequence '("C-x a" "C-x c" "C-x r"))

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
