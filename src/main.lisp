(defpackage pichunter
  (:use :cl :cl-who
	:easy-routes :postmodern)
  (:import-from :pichunter.std :slurp))

(in-package :pichunter)

(defparameter *server* (hunchentoot:start (make-instance 'easy-routes:routes-acceptor :port 3000)))
(defparameter *js-location* nil)

(cond ((string= (machine-instance)
		"roland")
       (setf *js-location* #P"/home/feuer/common-lisp/pichunter/frontend/elm.js"))
      
      ((string= (machine-instance)
		"vivacia")
       (setf *js-location* #P"/Users/feuer/Projects/pichunter/frontend/elm.js")))

(assert *js-location* nil "JS location should be configured in the source code when developing")

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @transaction (next)
  (with-connection '("pichunter" "pichunter" "TESTIPASSU" "localhost" :pooled-p t)
    (with-transaction ()
      (funcall next))))

(defparameter elm-init-script
"var app = Elm.Main.init({
    node: document.getElementById(\"app\")
});")

(defroute main ("/" :method :get
		    :decorators (@transaction))
    ()
  (let ((script (slurp *js-location*)))
    (format nil "<html> <head> <script> ~A </script> </head> <body> <div id=\"app\" /> <script> ~A </script> </body> </html>" script elm-init-script)))
