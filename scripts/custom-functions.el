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

