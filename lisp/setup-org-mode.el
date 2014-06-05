(require-package 'org)
(require 'org)

;; Custom Key Bindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c C-x I") 'alf/org-punch-in)
(global-set-key (kbd "C-c C-x O") 'alf/org-punch-out)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "C-c C-x SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-c C-x C-x") 'org-clock-in)

(setq org-mobile-inbox-for-pull "~/Dropbox/Org/refile.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq dropbox-dir (expand-file-name "~/Dropbox"))
(setq org-directory (file-name-as-directory
                     (expand-file-name "Org" dropbox-dir)))

(let ((default-directory org-directory))
  (setq org-agenda-files
        (mapcar 'expand-file-name
                (list "refile.org"
                      "todo.org"
                      "personal.org"
                      "journal.org"
                      "projects")))

  (setq org-default-notes-file (expand-file-name "refile.org"))

  (setq org-capture-templates
        `(("t" "todo" entry (file ,(expand-file-name "refile.org"))
           "* TODO %?\n	%i\n	%a" :clock-in t :clock-resume t)
          ("T" "todo under current clock" entry (clock)
           "* TODO %?\n	%i\n	%a" )
          ("c" "capture" entry (file ,(expand-file-name "refile.org"))
           "* %?\n	%i\n	%a" :clock-in t :clock-resume t)
          ("n" "note" entry (file ,(expand-file-name "refile.org"))
           "* %? :NOTE:\n	%a" :clock-in t :clock-resume t)
          ("i" "Interruptionion" entry (file ,(expand-file-name "refile.org"))
           "* %? :INTERRUPTION:\n	%i\n	%a" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+datetree ,(expand-file-name "journal.org"))
           "* %?\n	%i\n	%a" :clock-in t :clock-resume t)
          ("b" "Blog idea" entry (file+headline ,(expand-file-name "blog-ideas.org") "Blog ideas")
           "** %?\n%u\n")
          ("r" "respond" entry (file ,(expand-file-name "refile.org"))
           "* NEXT Respond to %:from on %:subject\n	SCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
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
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
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
                       (org-tags-match-list-sublevels nil)))
                (todo "NEXT"
                           ((org-agenda-overriding-header "Next tasks")
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Root tasks")
                            (org-agenda-skip-function #'(alf/skip-if 'bh/is-subproject-p))
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-PROJECT-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Tasks")
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "WAITING-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-todo-ignore-deadlines 'future)))
                (todo "HOLD"
                           ((org-agenda-overriding-header "Held Projects")

                            (org-agenda-sorting-strategy
                             '(priority-down category-keep)))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")
                (org-tags-match-list-sublevels nil)))
              ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")
                (org-agenda-todo-ignore-scheduled t)
                (org-agenda-todo-ignore-deadlines t)
                (org-agenda-todo-ignore-with-date t)
                (org-tags-match-list-sublevels t)
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
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
               (org-tags-match-list-sublevels nil)))))

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

;; Don't inherit the PROJECT tag since subtasks of a project should
;; not be considered a project
(add-to-list 'org-tags-exclude-from-inheritance "PROJECT")
;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
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

(setq org-startup-indented t)

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-log-done (quote time))
(setq org-log-into-drawer t)

(setq org-use-speed-commands t)

(defun alf/org-punch-in ()
  (interactive)
  (org-mobile-pull)
  (org-clock-in '(4)))

(defun alf/org-punch-out ()
  (interactive)
  (save-some-buffers)
  (org-mobile-push)
  (org-clock-out))

(defun alf/skip-if (pred)
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (funcall pred)
        subtree-end
      nil)))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

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

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 3))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-completion-use-helm t)

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "8:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00 8:00"))))

(setq org-agenda-span 'day)

(setq org-archive-location "%s_archive::* Archived Tasks")

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

(setq org-startup-with-inline-images t)

; Increase default number of headings to export
(setq org-export-headline-levels 6)

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; sdot - Graphviz DOT block
(define-skeleton skel-org-block-dot
  "Insert a org graphviz dot block, querying for filename."
  "File (no extension): "
  "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
  "graph G {\n"
  _ - \n
  "}\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; sditaa - Ditaa source block
(define-skeleton skel-org-block-ditaa
  "Insert a org ditaa block, querying for filename."
  "File (no extension): "
  "#+begin_src ditaa :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; selisp - Emacs Lisp source block
(define-skeleton skel-org-block-elisp
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

(setq org-agenda-include-diary nil)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

(setq org-read-date-prefer-future 'time)

(setq org-link-mailto-program (quote (compose-mail "%a" "%s")))

(setq org-src-fontify-natively t)

;; Disable C-c [ and C-c ] and C-c ; in org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c["    'undefined)
             (org-defkey org-mode-map "\C-c]"    'undefined)
             (org-defkey org-mode-map "\C-c;"    'undefined))
          'append)

(setq org-catch-invisible-edits 'smart)

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :narrow 80)))

(setq org-confirm-babel-evaluate nil)

(provide 'setup-org-mode)
