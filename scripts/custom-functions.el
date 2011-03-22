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

(defun matportalen-deploy ()
  "Uses fabric to compile widgets and deploy them."
  (interactive)
  (setq fabric-project-path "~/Projects/matportalen/")
  (fabric "deploy"))

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

(define-key global-map [(f10)] 'matportalen-deploy)
(define-key global-map [(f11)] 'matportalen-search-templates)

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

(defun open-index (id)
  (interactive "sEnter index-id: ")
  (let ((index-url (concat
		    "http://ecedemo:8080/indexer-webservice/index/"
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
		    "http://ecedemo:8080/solr/select?"
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

