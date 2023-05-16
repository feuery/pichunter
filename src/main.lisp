(defpackage pichunter
  (:use :cl :cl-who
	:easy-routes :postmodern))

(in-package :pichunter)

(defparameter *server* (hunchentoot:start (make-instance 'easy-routes:routes-acceptor :port 3000)))
(defparameter *js-location* nil)

(cond ((string= (machine-instance)
	       "roland")
       (setf *js-location* #P"/home/feuer/common-lisp/pichunter/frontend/elm.js")))

(assert *js-location* nil "JS location should be configured in the source code when developing")

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @transaction (next)
  ;; (with-connection '("blogdb" "blogadmin" "blog" "localhost" :pooled-p t)
  ;;   (with-transaction ()
  (funcall next));))

(defun slurp (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defparameter elm-init-script
"var app = Elm.Main.init({
    node: document.getElementById(\"app\")
});")

(defroute main ("/" :method :get
		    :decorators (@transaction))
    ()
  (let ((script (slurp *js-location*)))
    (format nil "<html> <head> <script> ~A </script> </head> <body> <div id=\"app\" /> <script> ~A </script> </body> </html>" script elm-init-script)))
