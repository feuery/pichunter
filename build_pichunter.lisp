(ql:quickload :pichunter)
(save-lisp-and-die "pichunter_server"
		   :toplevel #'pichunter:main
		   :executable t)
		   
