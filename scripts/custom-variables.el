;; Add support for mvn compilation errors, taken from
;; http://jroller.com/malformed/entry/emacs_maven_2
(add-to-list
 'compilation-error-regexp-alist-alist
 '(mvn "\\(^/.*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3 2 1))
(add-to-list
 'compilation-error-regexp-alist-alist
 '(mvn-warning "^\\[WARNING\\] \\(/.*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3 1 1))
(add-to-list 'compilation-error-regexp-alist 'mvn)
(add-to-list 'compilation-error-regexp-alist 'mvn-warning)
