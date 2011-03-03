(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(setq org-directory (file-name-as-directory
		     (expand-file-name "Org" dropbox-dir)))

(setq org-log-into-drawer "LOGBOOK")
(setq org-agenda-files
      (list (concat org-directory "todo.org")
            (concat org-directory "blog-ideas.org")
            (concat org-directory "note.org")
            (concat org-directory "journal.org")))

(setq org-capture-templates
      `(("b" "Blog idea" entry
         (file+headline ,(concat org-directory "blog-ideas.org") "Blog ideas")
         "** %?\n%u\n")
        ("t" "Todo" entry
         (file+headline ,(concat org-directory "todo.org") "Tasks")
         "* TODO %?\n %i\n")
        ("n" "Note" entry
         (file ,(concat org-directory "note.org"))
         "* %?\n %i\n")
        ("j" "Journal" entry
         (file+datetree ,(concat org-directory "journal.org"))
             "* %?\nEntered on %U\n  %i")))

(setq org-timer-default-timer 25)

(add-hook 'org-clock-in-hook
	  '(lambda()
	     (if (not
		  org-timer-current-timer)
		 (org-timer-set-timer '(16)))))

(add-hook 'org-mode-hook
	  '(lambda()
	     (setq fill-column 78)))
