(setq el-get-sources
      '((:name css-mode :type elpa)
	(:name smart-tab :type elpa)
	(:name magit :type elpa)
        (:name textmate
               :type git
               :url "http://github.com/defunkt/textmate.el"
               :load "textmate.el")
        (:name yaml-mode 
               :type git
               :url "http://github.com/yoshiki/yaml-mode.git"
               :features yaml-mode)))
(el-get 'sync)
