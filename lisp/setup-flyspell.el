(require-package 'flyspell)
(require 'flyspell)

(setq ispell-program-name "hunspell") 
(require-package 'rw-hunspell)
(require 'rw-hunspell)

(setq
   ispell-dictionary-alist
   '(("en-US"
      "[A-Za-z]" "[^A-Za-z]" "[\"]"
      nil ("-d" "en-US") nil utf-8)
     ("nb"
      "[A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[^A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[\"]"
      nil ("-d" "nb") nil utf-8)
     ("nn"
      "[A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[^A-Za-zçéêèóôòæøåÇÉÊÈÓÔÒÆØÅ]" "[\"]"
      nil ("-d" "nn") nil utf-8)))

(eval-after-load "ispell"
    (progn
      (setq ispell-dictionary "en-US"
	    ispell-extra-args '("-a" "-i" "utf-8") ; aspell doesn't understand -i utf-8, hunspell needs it
	    ispell-silently-savep t)))

(provide 'setup-flyspell)
