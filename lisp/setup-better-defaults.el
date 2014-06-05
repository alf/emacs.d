(require-package 'better-defaults)

;; I don't have time to write yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(add-to-list 'after-save-hook 'delete-trailing-whitespace)

;; Show week numbers in calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(provide 'setup-better-defaults)
