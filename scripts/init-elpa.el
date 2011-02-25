(require 'package)

(setq package-archives
      (cons '("tromey" . "http://tromey.com/elpa/") package-archives))
(package-initialize)

(add-to-list 'load-path (expand-file-name "el-get" plugins-dir))
(require 'el-get)

