(setq auto-mode-alist
	(cons '("\\.zcml\\'" . nxml-mode)
		auto-mode-alist))

(setq auto-mode-alist
	(cons '("\\.pt\\'" . nxml-mode)
		auto-mode-alist))

(add-hook 'python-mode-hook
	  (lambda()
            (setenv "LANG" "en_US.UTF-8")
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

(ad-activate 'python-calculate-indentation)

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
               '("\\.py\\'" flymake-check-init))

  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "jslint" (list local-file))))

  (setq flymake-err-line-patterns 
	(cons '("^  [[:digit:]]+ \\([[:digit:]]+\\),\\([[:digit:]]+\\): \\(.+\\)$"  
		nil 1 2 3)
	      flymake-err-line-patterns))
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jslint-init)))

(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files 
                (expand-file-name "plugins/html5-el/schemas.xml" user-emacs-directory)))

(require 'whattf-dt)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
