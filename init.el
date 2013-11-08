(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

; Keep this at the top in case I introduce an error in my config
; further down
(if (eq system-type 'darwin)
    (custom-set-variables
     '(ns-command-modifier 'meta)
     '(ns-right-command-modifier 'meta)
     '(ns-option-modifier nil)
     '(ns-right-option-modifier nil)))

(defun alf-load-if-exists (filename)
  (when (file-exists-p filename) (load filename)))

(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(alf-load-if-exists (concat user-emacs-directory "packages.el"))
(alf-load-if-exists (concat user-emacs-directory system-name ".el"))
(alf-load-if-exists (concat user-emacs-directory "secret-settings.el"))

(require 'flymake)

(define-compilation-mode fabric-mode "Fabric"
  "Specialization of compilation-mode for use with fabric"
  nil)

(defvar fabric-project-path nil
  "If set, run fabric using this path")

(defvar fabric-command nil
  "If set, run fabric with this command")

(defun fabric (command)
  (interactive
   (list
    (if (or current-prefix-arg (not fabric-command))
	(read-shell-command "Enter fabric command: ")
      fabric-command)))
  (if (not (eq command fabric-command))
      (setq fabric-command command))
  (let ((dir (or
	      fabric-project-path
	      default-directory)))
    (fabric-run dir command)))

(defun fabric-run (dir command)
  (with-current-buffer (get-buffer-create "*fab*")
    (fabric-mode)
    (cd dir)
    (let ((inhibit-read-only t)
	  (proc (start-process "fabric" "*fab*" "fab" command)))
      (set-process-sentinel proc 'compilation-sentinel)
      (set-process-filter proc 'compilation-filter)
      (set-marker (process-mark proc) (point-max))
      (run-hook-with-args 'compilation-start-hook proc)
      (setq compilation-in-progress
	    (cons proc compilation-in-progress))

      (buffer-disable-undo)
      (display-buffer "*fab*")
      (erase-buffer)
      (sit-for 0)
      (end-of-buffer))))

(defun matportalen-build ()
  "Uses fabric to build the project ."
  (interactive)
  (setq fabric-project-path "~/Projects/matportalen/")
  (fabric "build"))

(defun matportalen-search-templates ()
  "Uses fabric to sync the search templates"
  (interactive)
  (setq fabric-project-path "~/Projects/matportalen/")
  (fabric "search_templates"))

(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (push-mark end nil)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin (mark))
    (pop-mark))
  (message "Ah, much better!"))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(defun open-index (id)
  (interactive "sEnter index-id: ")
  (let ((index-url (concat
		    "http://localhost:8080/indexer-webservice/index/"
		    id)))
    (url-retrieve index-url
		  (lambda (s id)
		    (rename-buffer (generate-new-buffer-name (concat "* index: " id)))
		    (remove-headers)
		    (bf-pretty-print-xml-region 1 (point-max))
		    (pop-to-buffer (current-buffer)))
		  (list id))))

(defun solr-search (params)
  (interactive "sEnter params: ")
  (let ((search-url (concat
                     "http://localhost:8080/solr/select?"
                     (replace-regexp-in-string "wt=javabin" "wt=xml" params))))
    (url-retrieve search-url
		  (lambda (s)
		    (rename-buffer (generate-new-buffer-name "*search-result*"))
		    (remove-headers)
		    (bf-pretty-print-xml-region 1 (point-max))
		    (pop-to-buffer (current-buffer))))))

(defun remove-headers ()
  (goto-char (point-min))
  (re-search-forward "^$" nil 'move)
  (delete-region (point-min) (1+ (point))))

(defun alf/switch-dictionary ()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "norsk") "english" "norsk")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(defun alf/previous-frame ()
  (interactive)
  (other-frame -1))

(defun alf/previous-window ()
  (interactive)
  (other-window -1))

(require 'compile)
(setq compile-search-file nil)
(defun find-search-file ()
  ;; Search for the pom file traversing up the directory tree.
  (setq dir (expand-file-name default-directory))
  (let ((parent (file-name-directory (directory-file-name dir))))
    (while (and (not (file-readable-p (concat dir compile-search-file)))
                (not (string= parent dir)))
      (setq dir parent
            parent (file-name-directory (directory-file-name dir))))
    (if (string= dir parent)
        (error "Search file %s is missing" compile-search-file)
      (with-current-buffer outbuf
        (setq default-directory dir)))))

(setq compilation-process-setup-function 'find-search-file)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                               (enable-paredit-mode)))

(add-hook 'clojure-mode-hook (lambda ()
                               (define-key global-map "\C-\M-d" 'down-list)
                               (enable-paredit-mode)))
(add-hook 'java-mode-hook (lambda ()
                            (make-local-variable 'compile-search-file)
                            (setq compile-search-file "pom.xml")))

;; Add support for mvn compilation errors, taken from
;; http://jroller.com/malformed/entry/emacs_maven_2
(add-to-list 'compilation-error-regexp-alist 'mvn)
(add-to-list 'compilation-error-regexp-alist-alist
             '(mvn "\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*" 1 2 3))

(defun dired-open-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))

(add-hook 'dired-load-hook (lambda ()
                             (load "dired-x")
                             (define-key dired-mode-map "o" 'dired-open-mac)))

;; since I don't have a super-key use Ctrl-/ for comment/uncomment
(define-key global-map [(ctrl /)] 'comment-or-uncomment-region-or-line)

;; upgrade a few builtins
(define-key global-map "\C-x\C-b" 'ibuffer)
(define-key global-map "\C-x\C-d" 'ido-dired)

(add-hook 'ibuffer-mode-hooks '(lambda ()
				 (define-key ibuffer-mode-map "\C-x\C-f" 'ido-find-file)
				 (define-key ibuffer-mode-map [(shift return)] 'ibuffer-visit-buffer-other-window)))

;; Full screen is soooo lovely
(define-key global-map [(meta return)] 'ns-toggle-fullscreen)

;; Make it easier to switch between frames
(define-key global-map "\M-`" 'other-frame)
(define-key global-map "\M-~" 'alf/previous-frame)

;; Make it easier to switch back to previous window
(define-key global-map "\C-xO" 'alf/previous-window)

;; Map the norwegian characters for convenience
(define-key global-map [(super a)]  (kbd "å"))
(define-key global-map [(super A)]  (kbd "Å"))
(define-key global-map [(super o)]  (kbd "ø"))
(define-key global-map [(super O)]  (kbd "Ø"))
(define-key global-map [(super \')] (kbd "æ"))
(define-key global-map [(super \")] (kbd "Æ"))

;; window movement commands inspired by emacs
(define-key global-map "\C-z" nil)
(define-key global-map "\C-zh" 'windmove-left)
(define-key global-map "\C-zl" 'windmove-right)
(define-key global-map "\C-zk" 'windmove-up)
(define-key global-map "\C-zj" 'windmove-down)

(setq auto-mode-alist
      (cons '("\\.zcml\\'" . nxml-mode)
            auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.pt\\'" . nxml-mode)
            auto-mode-alist))

(add-hook 'python-mode-hook
	  (lambda()
	    (set (make-local-variable 'compile-command) (concat "python " (buffer-name)))))

(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Handle lines beginning with a closing bracket and indent them so that
they line up with the line containing the corresponding opening bracket."
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss)))
      (if (and (not (eq 'string (syntax-ppss-context syntax)))
               (python-continuation-line-p)
               (cadr syntax)
               (skip-syntax-forward "-")
               (looking-at "\\s)"))
          (progn
            (forward-char 1)
            (ignore-errors (backward-sexp))
            (setq ad-return-value (current-indentation)))
        ad-do-it))))

;(ad-activate 'python-calculate-indentation)

(when (load "flymake" t)
  (require 'tramp-cmds)
  (setq flymake-gui-warnings-enabled nil)

  (defun flymake-check-init ()
    ;; Make sure it's not a remote buffer or flymake would not work
    (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "check.py" (list local-file)))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-check-init)))

(setq dropbox-dir (expand-file-name "~/Dropbox"))
(setq org-directory (file-name-as-directory
		     (expand-file-name "Org" dropbox-dir)))

(let ((default-directory org-directory))
  (load (concat user-emacs-directory "org-mode.el"))
  (setq org-directory (file-name-as-directory
                       (expand-file-name "Org" dropbox-dir)))

  (setq org-agenda-files
	(mapcar 'expand-file-name
		(list "refile.org"
                      "todo.org"
                      "personal.org"
                      "journal.org"
		      "projects")))

  (setq org-default-notes-file (expand-file-name "refile.org"))
  (setq bh/organization-task-id "11F0AFCD-E153-4C75-B812-017523C75220")

  (setq org-capture-templates
        `(("t" "todo" entry (file ,(expand-file-name "refile.org"))
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file ,(expand-file-name "refile.org"))
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file ,(expand-file-name "refile.org"))
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree ,(expand-file-name "journal.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file ,(expand-file-name "refile.org"))
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file ,(expand-file-name "refile.org"))
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
              ("b" "Blog idea" entry (file+headline ,(expand-file-name "blog-ideas.org") "Blog ideas")
               "** %?\n%u\n"))))

;; $Revision: 1.1.1.1 $

;; Copyright (C) 2000 by Ingo Koch

;; Author: ingo Koch <ingo.koch@ikoch.de>
;; The Idea and a lot of the code is stolen from the
;; .emacs file of  Jake Donham <jake@bitmechanic.com>
;; available at http://www.jaked.org/emacs.html
;; who did this for the mocha java decompiler
;; Maintainer: Ingo Koch <ingo.koch@ikoch.de>
;; Keywords: java, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:

;; This package is an add-on to the Java Development Environment
;; (JDE) for Emacs. It automatically decompiles a class file and
;; offers you a buffer to view or edit it.
;; javadecomp (currently) relies on the jad java decompiler to
;; do the actual work, but it should be possible to extend it to
;; whatever you like (sugestions are welcome).
;; jad is available at the Jad home page:
;; http://www.geocities.com/SiliconValley/Bridge/8617/jad.html
;; by Pavel Kouznetsov (kpdus@yahoo.com).
;; It supports a wide range of OS like:
;; - Windows 95/NT on Intel platform
;; - Linux on Intel platform
;; - Solaris 7.0 on Intel platform
;; - Rhapsody 5.3 on PowerPC platform
;; - AIX 4.2 on IBM RS/6000 platform
;; - OS/2
;; - Solaris 2.5 on SUN Sparc platform
;; - FreeBSD 2.2.x

;;; Installation:

;; Put the following in your .emacs file:
;;   (require 'javadecomp)
;;
;;; Usage:
;; Open a class file and feel happy.

;;; Support:

;; Any comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to Ingo Koch at ingo.koch@ikoch.de.
;;

;;; Code:

(defvar jdc-command  "/usr/local/bin/jad"
  "The name of the decompiler if it's on your path, otherwise
a full qualified path to it.")

(defvar jdc-parameter  " -space -t2 "
  "Extra parameter which should be added to the call of the decompiler.")

(defvar jdc-extension  "jad"
  "The extension which is used for the generated java files.")

(defun jdc-find-file ()
  "Find a classfile and decompile it, opening the decompiled file instead."
  (interactive)
  (let ((jdc-classfile (read-file-name ".class file: ")))
    (find-file jdc-classfile)
    (jdc-buffer)
    (java-mode)))

(defun jdc-buffer ()
  "Construct the command for decompiling a class file, call the resulting
command and load the decompiled file."
  (let*
      (
       (jdc-classfile (file-name-nondirectory (buffer-file-name)))
       (jdc-javafile (concat (substring jdc-classfile 0 -5) jdc-extension))
       (command (concat jdc-command jdc-parameter jdc-classfile)))

    (shell-command command)
    (find-alternate-file jdc-javafile)))

;; a hook to be able to automatically decompile-find-file .class files
(add-hook
 'find-file-hooks
 (lambda ()
   (let ((file (buffer-file-name)))
     (cond ((string= (substring file -6) ".class")
	    (progn (jdc-buffer) (java-mode)))))))

(defun decompile-region ()
  (interactive)
  (let ((command (concat jdc-command jdc-parameter " -p /dev/stdin")))
    (shell-command-on-region (region-beginning) (region-end) command (current-buffer) t)))

(defun decompile-buffer ()
  (interactive)
  (beginning-of-buffer)
  (set-mark (point-max))
  (decompile-region))

(provide 'javadecomp)

(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 20)
  (set-frame-size (selected-frame) 270 76))

(defun my-js-mode-hook ()
  (imenu-add-menubar-index)
  ;; Activate the folding mode
  (hs-minor-mode t))

(add-hook 'js-mode-hook 'my-js-mode-hook)

(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
       ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                     (replace-regexp-in-string ".*1G.*3G" "> " output))))))

(fset 'yes-or-no-p 'y-or-n-p)

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

;; Enable Semantic
(semantic-mode 1)

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.java\" | /usr/local/bin/etags --append -" dir-name)))

(define-key ac-mode-map (kbd "M-/") 'auto-complete)

;; Add ac-complete-filename to ac-sources for all buffers
(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-complete-filename))

(add-to-list 'eclim--file-coding-system-mapping '("iso-latin-1-dos" . "iso-8859-1"))

(load "~/.ercpass")
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords `((freenode (("alfborge" . ,freenode-alfborge-pass)))))

(defun alf/unhex-region ()
  (interactive)
  (let ((hexxed-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (url-unhex-string hexxed-string))))

(defun alf/hex-region ()
  (interactive)
  (let ((unhexxed-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (replace-in-string (url-hexify-string unhexxed-string) "%0a" "\n"))))

(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(setq alf/brightstar-subject-keyword
      `(("<li> *{{\\([^}]*\\)"
         (0 (progn (compose-region (match-beginning 1)
                                   (match-end 1) "\u2026")
                   nil)))))

(defun alf/hide-subject ()
  (interactive)
  (font-lock-add-keywords major-mode alf/brightstar-subject-keyword)
  (font-lock-refresh-defaults))

(defun alf/show-subject ()
  (interactive)
  (font-lock-remove-keywords major-mode alf/brightstar-subject-keyword)
  (font-lock-refresh-defaults))

(defun alf/magit-log-folder (dir-name)
  (interactive "DDirectory :")
  (setq magit-refresh-args `(oneline "HEAD" ("--graph" "--" ,dir-name)))
  (magit-refresh))

(setq dired-dwim-target t)
(put 'set-goal-column 'disabled nil)

(if (featurep 'ns)
    (progn
      (defun ns-raise-emacs ()
        "Raise Emacs."
        (ns-do-applescript "tell application \"Emacs\" to activate"))

      (if (display-graphic-p)
          (progn
            (add-hook 'server-visit-hook 'ns-raise-emacs)
            (add-hook 'before-make-frame-hook 'ns-raise-emacs)
            (ns-raise-emacs)))))

(defun ns-toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(defun alf/reflow-sparql ()
  (interactive)
  (save-excursion
    (replace-string "}" "\n}" nil (region-beginning) (region-end))
    (replace-string "{" "\n{\n" nil (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))))
