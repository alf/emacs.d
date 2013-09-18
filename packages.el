
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

;; set local recipes
(setq
 el-get-sources
 '((:name smex				; a better (ido like) M-x
	  :after (progn
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (define-key global-map (kbd "M-x") 'smex)
		   (define-key global-map (kbd "M-X") 'smex-major-mode-commands)))
   (:name magit                        ; git meet emacs, and a binding
	  :after (progn
		   (define-key global-map (kbd "C-x C-z") 'magit-status)))
   (:name wanderlust)
   (:name emacs-w3m)
   (:name monky
          :type git
          :url "https://github.com/ananthakumaran/monky.git")
   (:name color-theme-solarized
	  :after (progn
		   (color-theme-solarized-light)))
   (:name python)
   (:name zencoding-mode)
   (:name virtualenv)
   (:name textmate
          :after (progn
                   ;; I prefer using meta-t for the textmate-stuff
                   (add-hook 'textmate-mode-hook
                             '(lambda ()
                                (add-to-list '*textmate-project-roots* ".bzr")
                                (add-to-list '*textmate-project-roots* "pom.xml")
                                (define-key *textmate-mode-map* [(ctrl \;)] 'textmate-goto-file)))
                   (textmate-mode)))
   (:name flymake-point)
   (:name ace-jump-mode
          :after (progn
                   (define-key global-map (kbd "C-x x") 'ace-jump-mode)))
   (:name expand-region
          :type git
          :url "https://github.com/magnars/expand-region.el"
          :after (progn
                   (define-key global-map (kbd "C-=") 'er/expand-region)
                   (define-key global-map (kbd "C-M-=") 'er/contract-region)))
   (:name auto-complete)
   (:name auto-complete-css)
   (:name auto-complete-etags)
   (:name auto-complete-yasnippet)
   (:name vcl-mode
          :url "https://www.varnish-cache.org/svn/trunk/varnish-tools/emacs/vcl-mode.el")
   (:name ac-slime)
   (:name ack)
   (:name paredit)
   (:name textile-mode)
   (:name dired-details)
   (:name n3-mode)
   (:name csharp-mode)
   (:name wgrep)
   (:name multiple-cursors)
   (:name graphviz-dot-mode
          :after (progn
                   (add-hook 'graphviz-dot-mode-hook
                             '(lambda ()
                                (setq compilation-read-command nil)))))
   (:name sparql-mode
          :type git
          :url "https://github.com/candera/sparql-mode.git"
          :after (progn
                   (autoload 'sparql-mode "sparql-mode.el"
                     "Major mode for editing SPARQL files" t)
                   (add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))))
   (:name org-mode)
   (:name org-jira
          :type git
          :url "https://github.com/baohaojun/org-jira.git"
          :after (progn
                   (add-hook 'org-jira-mode-hook
                             '(lambda ()
                                (setq jiralib-url "https://jira.bouvet.no")
                                (define-key org-jira-entry-mode-map "\C-cc" 'org-capture)))))
   (:name emacs-eclim
          :type git
          :url "https://github.com/senny/emacs-eclim.git"
          :after (progn
                   (require 'eclim)
                   (require 'eclimd)
                   (require 'ac-emacs-eclim-source)
                   (setq eclim-auto-save t)
                   (global-eclim-mode)

                   (add-hook 'eclim-mode-hook (lambda ()
                                                (add-to-list 'ac-sources 'ac-source-emacs-eclim)
                                                (add-to-list 'ac-sources 'ac-source-emacs-eclim-c-dot)))))
   (:name yasnippet
          :type git
          :url "https://github.com/capitaomorte/yasnippet.git")
   (:name elein)
   (:name clojure-mode
          :after (progn
                   (enable-paredit-mode)))
   (:name nrepl)
   (:name js-comint)))

(setq my-packages
      (mapcar 'el-get-source-name el-get-sources))

(el-get 'sync my-packages)
