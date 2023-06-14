(defpackage pichunter
  (:use :cl :cl-who :postmodern )
  (:import-from :pichunter.std :slurp)
  (:import-from :pichunter.decorators :@json :@transaction))

(in-package :pichunter)

(defparameter *js-location* nil)

(cond ((string= (machine-instance)
		"roland")
       (setf *js-location* #P"/home/feuer/common-lisp/pichunter/frontend/elm.js"))
      
      ((string= (machine-instance)
		"vivacia.local")
       (setf *js-location* #P"/Users/feuer/Projects/pichunter/frontend/elm.js")))

(assert *js-location* nil "JS location should be configured in the source code when developing")





(defparameter elm-init-script
"var app = Elm.Main.init({
    node: document.getElementById(\"app\")
});")

(defun get-picture (guid)
  (multiple-value-bind (picture-data mime) (pichunter.file-handler:get-picture-data guid)
    `(200 (:content-type ,mime) ,picture-data)))

(defun extract-pic-guid (path-info)
  (let* ((pattern "/api/pictures/(.*)$"))
    (ppcre:register-groups-bind (guid) (pattern path-info)
      guid)))

(defun handler (env)
  (destructuring-bind (&key request-method path-info request-uri
                         query-string headers
			 content-type content-length raw-body 
		       &allow-other-keys)
      env
    
    (format t "path-info: ~a~%" path-info)
    (if (string= path-info "/")
	`(200 nil (,(let ((script (slurp *js-location*)))
		      (format nil "<html> <head> <script> ~A </script> </head> <body> <div id=\"app\" /> <script> ~A </script> </body> </html>" script elm-init-script))))
	(if (string= path-info "/api/pictures")
	    `(200 nil ,(pichunter.file-handler:handle-upload (http-body:parse content-type content-length raw-body)))

	    (if (str:starts-with? "/api/pictures/" path-info)
		(let ((guid (extract-pic-guid path-info)))
		  (get-picture guid))

		`(204 nil nil))))))
		 

(defvar *clack-server*
  (clack:clackup (lambda (env)
		   (funcall 'handler env))
		 :port 3000))

;; (defroute main ("/" :method :get
;; 		    :decorators (@transaction))
;;     ()
;;   )
