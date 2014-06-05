;; Bookmarklet to load skewer:
;;
;;     javascript:(function(){var d=document ;var s=d.createElement('script');s.src='http://localhost:8023/skewer';d.body.appendChild(s);})()
;;

(require-package 'skewer-mode)
(require 'skewer-repl)
(require 'skewer-html)
(require 'skewer-css)
(autoload 'skewer-start "setup-skewer" nil t)
(autoload 'skewer-demo "setup-skewer" nil t)

(defun skewer-start ()
  (interactive)
  (let ((httpd-port 8023))
    (httpd-start)
    (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

(defun skewer-demo ()
  (interactive)
  (let ((httpd-port 8024))
    (run-skewer)
    (skewer-repl)))

(provide 'setup-skewer)
