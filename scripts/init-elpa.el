(add-to-list 'load-path (expand-file-name "el-get" emacs-dir))

(require 'el-get)
(require 'package)

(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
