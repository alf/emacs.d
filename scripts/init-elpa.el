(require 'package)
(setq package-archives
      (cons '("tromey" . "http://tromey.com/elpa/") package-archives))
(package-initialize)

(add-to-list 'load-path (expand-file-name "el-get" plugins-dir))
(require 'el-get)

(setq el-get-sources
      '((:name css-mode :type elpa)
        (:name textmate
               :type git
               :url "http://github.com/defunkt/textmate.el"
               :load "textmate.el")
        (:name yaml-mode 
               :type git
               :url "http://github.com/yoshiki/yaml-mode.git"
               :features yaml-mode)))
(el-get 'sync)
