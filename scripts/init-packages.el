(setq el-get-sources
      '((:name css-mode :type elpa)
	(:name smart-tab
	       :type elpa
	       :after (lambda()
			(require 'smart-tab)
			(global-smart-tab-mode t)
			(setq smart-tab-using-hippie-expand t)))
	(:name magit :type elpa)
	(:name autopair
	       :type elpa
	       :after (lambda()
			(require 'autopair)
			(autopair-global-mode t)))
	(:name diff-mode- 
	       :type emacswiki
	       :features diff-mode-)
	(:name edit-server
	       :type http
	       :url "https://github.com/stsquad/emacs_chrome/raw/master/servers/edit-server.el"
	       :after (lambda ()
			(require 'edit-server)
			(edit-server-start)))
	;; (:name vimpulse
	;;        :type git
	;;        :url "http://git.gitorious.org/vimpulse/vimpulse.git"
	;;        :info "vimpulse for emacs"
	;;        :features vimpulse)
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
