(defpackage pichunter
  (:use :cl :postmodern )
  (:import-from :pichunter.std :slurp)
  (:import-from :pichunter.routes :defroute)
  (:import-from :pichunter.decorators :@json :@transaction))

(in-package :pichunter)

(defparameter *js-location* nil)
(defparameter *css-location* nil)

(cond ((string= (machine-instance)
		"roland")
       (setf *js-location* #P"/home/feuer/common-lisp/pichunter/frontend/elm.js")
       (setf *css-location* #P"/home/feuer/common-lisp/pichunter/frontend/site.css"))
      
      ((string= (machine-instance)
		"vivacia.local")
       (setf *js-location* #P"/Users/feuer/Projects/pichunter/frontend/elm.js")
       (setf *css-location* #P"/Users/feuer/Projects/pichunter/frontend/site.css")))

(assert *js-location* nil "JS location should be configured in the source code when developing")



(defun get-picture (guid)
  (multiple-value-bind (picture-data mime) (pichunter.file-handler:get-picture-data guid)
    `(200 (:content-type ,mime) ,picture-data)))

(defun extract-pic-guid (path-info)
  (let* ((pattern "/api/pictures/(.*)$"))
    (ppcre:register-groups-bind (guid) (pattern path-info)
      guid)))

(defroute "get" "/site.css"
    env
  
  `(200 (:content-type "text/css")
	(,(slurp *css-location*))))

(defun js-helper-script ()
  (slurp (pathname (format nil "/~{~a/~}pichunter-helper.js" (cdr (pathname-directory *js-location*))))))

(defroute "get" :fallback env
  `(200 nil (,(let ((script (slurp *js-location*)))
		(format nil "<!DOCTYPE html>~%<html> <head> <meta charset=\"utf-8\" /> <link href=\"site.css\" rel=\"stylesheet\"/> <script> ~A </script> </head> <body> <div id=\"app\" /> <script> ~A </script> </body> </html>" script (js-helper-script))))))

(defroute "get" "/api/pictures/[\\w-]+" env
  (destructuring-bind (&key path-info &allow-other-keys) env
    (let ((guid (extract-pic-guid path-info)))
	  (get-picture guid))))

(defun handler (env)
  (destructuring-bind (&key request-method request-uri 
		       &allow-other-keys)
      env

    (let* ((urls (pichunter.std:hash-keys
		  (gethash (string-upcase request-method)
			   pichunter.routes:*routes*)))
	   (correct-url (some (lambda (url-regex)
				(if (and (stringp url-regex)
					 (cl-ppcre:scan-to-strings url-regex request-uri ))
				    url-regex))
			      urls))
	   (method-handlers (gethash (string-upcase request-method)
				     pichunter.routes:*routes*))
	   (handler (gethash (or correct-url :fallback) method-handlers)))
      (if handler
	  (funcall handler env)
	  `(400 nil (,(format nil "No handler found for ~a" request-uri)))))))

(defvar *clack-server*
  (clack:clackup
   (lack:builder
    :session
    (lambda (env)
      (funcall 'handler env)))
    :port 3000))
