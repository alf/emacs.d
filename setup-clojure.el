(require-package 'cider)
(require-package 'cider-tracing)
(require-package 'ac-cider-compliment)
(require-package 'clojure-mode)

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

;; Cider configuration
(require 'cider)
(setq nrepl-hide-special-buffers t
      cider-repl-pop-to-buffer-on-connect nil
      cider-popup-stacktraces nil
      cider-repl-popup-stacktraces t)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; ;; Use cider as the clojure execution backend
;; (declare-function nrepl-send-string-sync "ext:nrepl" (code &optional ns))

;; (defun org-babel-execute:clojure (body params)
;;   "Execute a block of Clojure code with Babel."
;;   (with-temp-buffer
;;     (insert (org-babel-expand-body:clojure body params))
;;     ((lambda (result)
;;        (let ((result-params (cdr (assoc :result-params params))))
;;          (if (or (member "scalar" result-params)
;;                  (member "verbatim" result-params))
;;              result
;;            (condition-case nil (org-babel-script-escape result)
;;              (error result)))))
;;      (plist-get (nrepl-send-string-sync
;;                  (buffer-substring-no-properties (point-min) (point-max))
;;                  (cdr (assoc :package params)))
;;                 :value))))

(provide 'setup-clojure)
