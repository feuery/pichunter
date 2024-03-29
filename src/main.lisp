(defpackage pichunter
  (:use :cl :postmodern :binding-arrows)
  (:export :main :*server* :start-server)
  (:import-from :pichunter.migrations :migrate)
  (:import-from :pichunter.std :slurp :slurp-utf-8)
  (:import-from :easy-routes :defroute)
  (:import-from :pichunter.file-handler :get-picture-data)
  (:import-from :pichunter.decorators :@json :@transaction))

(in-package :pichunter)

(defparameter *js-location* #P"pichunter.min.js")
(defparameter *css-location* #P"site.css")

(setf hunchentoot:*catch-errors-p* nil)

(cond ((string= (machine-instance)
		"roland")
       (setf *js-location* #P"/home/feuer/common-lisp/pichunter/frontend/elm.js")
       (setf *css-location* #P"/home/feuer/common-lisp/pichunter/frontend/site.css"))
      
      ((string= (machine-instance)
		"vivacia.local")
       (setf *js-location* #P"/Users/feuer/Projects/pichunter/frontend/elm.js")
       (setf *css-location* #P"/Users/feuer/Projects/pichunter/frontend/site.css")))

;; TODO load this from config or somewhere
;; (assert *js-location* nil "JS location should be configured in the source code when developing")

(defroute css ("/site.css" :method :get) ()
  (setf (hunchentoot:content-type*) "text/css")
  (slurp *css-location*))

(defun js-helper-script ()
  (slurp-utf-8 (pathname (format nil "/~{~a/~}pichunter-helper.js" (cdr (pathname-directory *js-location*))))))


(defun get-frontend ()
  (let ((script (slurp-utf-8 *js-location*)))
    (format nil "<!DOCTYPE html>~%
<html>
  <head>
    <meta charset=\"utf-8\" />
    <link href=\"/site.css\" rel=\"stylesheet\"/>
    <link rel=\"stylesheet\" href=\"https://unpkg.com/leaflet@1.9.4/dist/leaflet.css\" integrity=\"sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY=\" crossorigin=\"\"/>
    <script src=\"https://unpkg.com/leaflet@1.9.4/dist/leaflet.js\"> </script>
    <script src=\"https://unpkg.com/arrive@latest/minified/arrive.min.js\"> </script>
    <script src=\"https://unpkg.com/exifreader@latest/dist/exif-reader.js\"> </script>

    <script> ~A
    </script>
  </head>
  <body>
    <div id=\"app\" />
    <script> ~A </script>
  </body>
</html>" script (js-helper-script))))



 

(defroute root-mdmgr ("/admin/media" :method :get) ()
  (get-frontend))
(defroute root-grmgr ("/admin/usersgroups" :method :get) ()
  (get-frontend))
(defroute root-profilesettings ("/user_details" :method :get) ()
  (get-frontend))
(defroute root-spela-location ("/play/locationguessing" :method :get) ()
  (get-frontend))
(defroute root-spela-pictures ("/play/pictureguessing" :method :get) ()
  (get-frontend))
(defroute root ("/" :method :get) ()
  (get-frontend))




(defvar *server* nil)

(defun start-server (&key (port 3000))
  (format t "Starting pichunter server~%")
  (let ((server (make-instance 'easy-routes:easy-routes-acceptor :port port)))
    (when (equalp 3000 port)
      (setf *server* server))
    (hunchentoot:start server)
    (format t "Started pichunter server on ~a ~%" port)
    server))

(defun filename (path)
  (format nil "~a.~a"
	  (pathname-name path)
	  (pathname-type path)))

(defun import-pictures-from-fs ()
  (let ((imported-filenames (pichunter.file-handler:imported-filenames)))
    (->> 
      (uiop:directory-files #P"/etc/pichunter/pictures/")
      (remove-if-not #'pichunter.game-routes:get-position)
      (remove-if (lambda (path)
		   (member (filename path) imported-filenames :test #'string=)))
      
      (mapcar (lambda (path)
		(let ((filename (filename path)))
		  (format t "importing ~a~%" filename)
		  (handler-case 
		      (pichunter.file-handler:import-picture filename "image/jpeg" path)
		    (error (c)
		      (format t "import error: ~a~%" c)))))))))


		     

(defun main (&key (port 3000))
  (pichunter.std:with-db
      (with-schema (:pichunter)
	(migrate)
	(pichunter.game-routes:load-codesets)
	(import-pictures-from-fs)

	(when (pichunter.std:e2e?)
	  (pichunter.e2e:setup-e2e))))
  (start-server :port port)
  (handler-case
      (loop do (sleep 1000))
    (condition () nil)))
  
    
