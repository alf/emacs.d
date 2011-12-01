(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(setq org-directory (file-name-as-directory
		     (expand-file-name "Org" dropbox-dir)))

(let ((default-directory org-directory))
  (setq org-agenda-files
	(mapcar 'expand-file-name
		(list "inbox.org"
                      "journal.org"
                      "projects")))
		      

  (setq org-capture-templates
	`(("c" "Inbox" entry
	   (file ,(expand-file-name "inbox.org"))
	   "* TODO %?\n %i\nEntered on %U")
	  ("j" "Journal" entry
	   (file+datetree ,(expand-file-name "journal.org"))
	   "* %?\nEntered on %U\n  %i")
	  ("b" "Blog idea" entry
	   (file+headline ,(expand-file-name "blog-ideas.org") "Blog ideas")
	   "** %?\n%u\n"))))

(setq org-timer-default-timer 25)

(add-hook 'org-mode-hook
	  '(lambda()
	     (org-indent-mode)
	     (setq fill-column 78)))


;; From http://doc.norang.ca/org-mode.html#Clocking
;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
;; Yes it's long... but more is better ;)
(setq org-clock-history-length 28)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task state to NEXT when clocking in
(setq org-clock-in-switch-to-state (quote bh/clock-in-to-started))
;; Separate drawers for clocking and logs
(setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK"))
(setq org-startup-folded "content")
(setq org-todo-keywords 
      '((sequence "TODO(t)" "STARTED(s!/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "|" "DONE(d@/!)" "DEFERRED" "CANCELLED(c@)")))

;; Save clock data in the CLOCK drawer and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer "CLOCK")
(setq org-log-into-drawer "LOGBOOK")
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist (quote history))
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in ()
  (interactive)
  (setq bh/keep-clock-running t)
  (org-agenda nil "c"))

(defun bh/clock-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out)))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running (not org-clock-clocking-in) (marker-buffer org-clock-default-task))
    (bh/clock-in-default-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
