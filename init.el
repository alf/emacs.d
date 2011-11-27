(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

;; set local recipes
(setq
 el-get-sources
 '((:name evil
	  :after (lambda ()
		   (evil-mode 1)))
   (:name smex				; a better (ido like) M-x
	  :after (lambda ()
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit				; git meet emacs, and a binding
	  :after (lambda ()
		   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change		; move pointer back to last change
	  :after (lambda ()
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

(el-get 'sync)

(let ((alf-system-file (concat user-emacs-directory system-name ".el"))
      (alf-secret-file (concat user-emacs-directory "secret-settings.el")))
  (when (file-exists-p alf-system-file) (load alf-system-file))
  (when (file-exists-p alf-secret-file) (load alf-secret-file)))

(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))

(load "custom-functions.el")
(load "init-bindings.el") ;; depends on custom-functions.el

(load "init-package.el")
(load "init-modes.el")
(load "init-org.el")
(load "init-autoloads.el")

;; TODO Find out where the code in here should be.  Currently it's
;; just maven stuff
(load "custom-variables.el")
