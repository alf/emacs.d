(setenv "WORKON_HOME" "/Users/alf/.venvs")
(require-package 'elpy)
(elpy-enable)
(require-package 'jedi)

(defun elpy-nav-forward-definition ()
  "Move forward to the next definition (class or function)."
  (interactive)
  (if (save-excursion
        (forward-char 1)
        (re-search-forward "^ *\\(def\\|class\\) " nil t))
      (goto-char (match-beginning 1))
    (goto-char (point-max))))
(setq elpy-rpc-backend "jedi")

(defun elpy-nav-backward-definition ()
  "Move backward to the previous definition (class or function)."
  (interactive)
  (if (save-excursion
        (forward-char -1)
        (re-search-backward "^ *\\(def\\|class\\) " nil t))
      (goto-char (match-beginning 1))
    (goto-char (point-min))))

(define-key elpy-mode-map (kbd "M-n") 'elpy-nav-forward-definition)
(define-key elpy-mode-map (kbd "M-p") 'elpy-nav-backward-definition)

(provide 'setup-python)
