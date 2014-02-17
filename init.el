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
     ;flx
     ;flx-ido
     css-eldoc
     yasnippet
     smartparens
     ;ido-vertical-mode
     ;ido-at-point
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
     cider-tracing
     dot-mode
     session
     auto-complete)))

(init--install-packages)

(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

(require 'setup-org-mode)
(require 'setup-magit)
(autoload 'skewer-start "setup-skewer" nil t)
(autoload 'skewer-demo "setup-skewer" nil t)

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require-package 'ace-jump-mode)
(global-set-key (kbd "C-x SPC")  'ace-jump-mode)

(require-package 'move-text)
(move-text-default-bindings)

(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(eval-after-load 'eshell '(require 'setup-eshell))
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'setup-mu4e)
(require 'setup-smartparens)
(require 'setup-keybindings)

(require-package 'sparql-mode)

(require-package 'csharp-mode)
(require-package 'projectile)
(put 'set-goal-column 'disabled nil)
(require-package 'goto-last-change)
(global-set-key "\C-x\C-\\" 'goto-last-change)
(put 'narrow-to-region 'disabled nil)

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

(setq ido-everywhere nil)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
(ido-mode nil)
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/guide-key-sequence '("C-x a" "C-x c" "C-x r"))
