(require-package 'flyspell)
(require 'flyspell)

(setq ispell-program-name "hunspell") 
(require-package 'rw-hunspell)
(require 'rw-hunspell)

(setq
   ispell-dictionary-alist
   '((nil				; default 
      "[A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[^A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[\"]"
      nil ("-d" "/Users/username/.enchant/myspell/nb_NO") nil utf-8)
     ("nynorsk"
      "[A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[^A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[\"]"
      nil ("-d" "/Users/username/.enchant/myspell/nn_NO") nil utf-8)
     ("bokmål"
      "[A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[^A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[\"]"
      nil ("-d" "/Users/username/.enchant/myspell/nb_NO") nil utf-8)))

(eval-after-load "ispell"
    (progn
      (setq ispell-dictionary "bokmål"
	    ispell-extra-args '("-a" "-i" "utf-8") ; aspell doesn't understand -i utf-8, hunspell needs it
	    ispell-silently-savep t)))

(provide 'setup-flyspell)
