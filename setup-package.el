(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(defun packages-install (packages)
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-install package))))

(provide 'setup-package)
