(setq el-get-sources
      '((:name css-mode :type elpa)
	(:name smart-tab :type elpa)
	(:name magit :type elpa)
	(:name autopair :type elpa)
	(:name diff-mode- 
	       :type http
	       :url "http://www.emacswiki.org/emacs/download/diff-mode-.el")
	(:name vimpulse
	       :type git
	       :url "http://gitorious.org/vimpulse/vimpulse.git"
	       :info "vimpulse for emacs"
	       :features vimpulse)
	(:name nxhtml
	       :type git
	       :url "http://github.com/emacsmirror/nxhtml.git"
	       :load "autostart.el")
	(:name textmate
               :type git
               :url "http://github.com/defunkt/textmate.el"
               :load "textmate.el")
        (:name yaml-mode 
               :type git
               :url "http://github.com/yoshiki/yaml-mode.git"
               :features yaml-mode)))
(el-get 'sync) 
