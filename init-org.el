;;; init-org.el --- org-mode configuration.

;;; Commentary:

;; My org-mode setup

;;; Code:

(require 'org)

(use-package ob-restclient
  :commands org-babel-execute:restclient)

;; Standard keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Make sure my keymaps are available
;;; My very own prefix key, backgrounding emacs makes no sense
(bind-keys :prefix-map alf/ctrl-z-map
           :prefix "C-z"
	   ("p"     . alf/org-punch-dwim)
	   ("j"     . org-clock-goto)
	   ([? ] . bh/clock-in-last-task))

;; Setup org-mobile
(setq org-mobile-inbox-for-pull "~/Dropbox/Org/refile.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Store my org-mode files in dropbox
(setq dropbox-dir (expand-file-name "~/Dropbox"))
(setq org-directory (file-name-as-directory
                     (expand-file-name "Org" dropbox-dir)))

;; Make expand-file-name operate in my org-directory
(let ((default-directory org-directory))
  (setq org-agenda-files
        (mapcar 'expand-file-name
                (list "refile.org" ; Captured items go here
                      "work.org" ; Notes and tasks for work
                      "personal.org" ; Personal items
                      "journal.org" ; This is where I log thoughts and what I do
                      "projects"))) ; This folder contains my project files

  ;; This causes org-capture to store things in this file
  (setq org-default-notes-file (expand-file-name "refile.org"))

  ;; My various capture templates
  (setq org-capture-templates
        `(("t" "todo" entry (file ,(expand-file-name "refile.org"))
           "* TODO %?\n\t%a\n%i" :clock-in t :clock-resume t)
          ("T" "todo under current clock" entry (clock)
           "* TODO %?\n\t%a\n%i" )
          ("c" "capture" entry (file ,(expand-file-name "refile.org"))
           "* %?\n\t%a\n%i" :clock-in t :clock-resume t)
          ("C" "capture under current clock" entry (clock)
           "* %?\n\t%a\n%i" )
          ("n" "note" entry (file ,(expand-file-name "refile.org"))
           "* %? :NOTE:\n\t%a" :clock-in t :clock-resume t)
          ("i" "Interruptionion" entry (file ,(expand-file-name "refile.org"))
           "* %? :INTERRUPTION:\n\t%a\n%i" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+datetree ,(expand-file-name "journal.org"))
           "* %?\n\t%a\n%i" :clock-in t :clock-resume t)
          ("b" "Blog idea" entry (file+headline ,(expand-file-name "blog-ideas.org") "Blog ideas")
           "** %?\n%u\n")
          ("r" "respond" entry (file ,(expand-file-name "refile.org"))
           "* TODO Respond to %:from on %:subject\n\tSCHEDULED: %t\n%U\n\t%a\n" :clock-in t :clock-resume t :immediate-finish t))))

;; My workflow
(setq org-todo-keywords
      (quote ((sequence "TODO(t/!)" "STARTED(s/!)" "WAITING(w@/!)" "|" "DONE(d/!)")
              (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")
                (org-tags-match-list-sublevels nil)))
              ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
               ((org-agenda-overriding-header "Tasks")
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "PROJECT-HOLD-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" tags-todo "-HOLD-CANCELLED+WAITING/!"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-tags-match-list-sublevels nil))
              ("u" "Unscheduled Tasks" alltodo ""
               ((org-agenda-overriding-header "Tasks")
                (org-agenda-skip-function
                 (lambda nil
                   (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))))))))

; Tags with fast selection keys
(setq org-tag-alist (quote (("PROJECT" . ?p)
                            ("PHONE" . ?p)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("ORG" . ?O)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

;; Don't show done tasks in the agenda
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; Don't bother showing the past, the future is more interesting
;; We use reporting for the past
(setq org-agenda-start-on-weekday nil)

;; When at the beginning of a headline, speed commands allow for quick navigation
(setq org-use-speed-commands t)

;; Don't inherit the PROJECT tag since subtasks of a project should
;; not be considered a project
(add-to-list 'org-tags-exclude-from-inheritance "PROJECT")

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; Org-mode should be smarter in headlines
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)


;; Write a small message when marking something as done
(setq org-log-done 'note)
(setq org-log-into-drawer t)

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))


;; Use full outline paths for refile targets - makes it easier with helm
(setq org-refile-use-outline-path t)
;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-helm t)

(setq org-blank-before-new-entry '((heading . always) (plain-list-item . auto)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "8:00"
                            :min-duration 0
                            :max-gap 0
                            :gap-ok-around ("4:00"))))

;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties
      (quote (("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00 8:00"))))

(setq org-archive-location "%s_archive::* Archived Tasks")

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :narrow 80)))


(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (python . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (java . t)
         (plantuml . t)
         (restclient . t)
         (latex . t))))

(setq org-startup-with-inline-images t)


;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

(setq org-read-date-prefer-future 'time)

(setq org-src-fontify-natively t)

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun alf/org-punch-dwim ()
  (interactive)
  (if (org-clock-is-active)
      (alf/org-punch-out)
    (alf/org-punch-in)))

(defun alf/org-punch-in ()
  (interactive)
  (org-mobile-pull)
  (bh/clock-in-task-by-id "d4913cba-f557-4315-ad43-306f801b77c5")
  (org-agenda nil " "))

(defun alf/org-punch-out ()
  (interactive)
  (org-clock-out)
  (save-some-buffers)
  (org-mobile-push))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

;; Setup org-latex
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f"))


(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("bouvet"
               "\\documentclass[norsk,10pt,a4paper]{article}
\\usepackage{bouvet}
\\usepackage[normalem]{ulem}
\\usepackage{wrapfig}
\\usepackage{csquotes}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;; init-org.el ends here
