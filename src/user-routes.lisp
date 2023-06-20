(defpackage pichunter.user-routes
  (:use :cl :postmodern :json-mop)
  (:import-from :pichunter.std :slurp-string-body :with-db :sha-512)
  (:import-from :pichunter.routes :defroute))

(in-package :pichunter.user-routes)


(defclass register-form ()
  ((displayname :initarg :displayname
		:json-type :string
		:json-key "displayname")
   (username :initarg :username
	     :json-type :string
	     :json-key "username")
   (password :initarg :password
	     :json-type :string
	     :json-key "password")
   (password-again :initarg :password-again
		   :json-type :string
		   :json-key "password-again"))
  (:metaclass json-serializable-class))


;; (let ((obj (make-instance 'register-form :displayname "på"
;; 					 :username "feuer"
;; 					 :password "lol"
;; 					 :password-again "lol")))
;;   (json-to-clos (with-output-to-string (s)
;; 		  (encode obj s))
;; 		'register-form))



(defroute "post" "/register"
    env  
  (destructuring-bind (&key raw-body &allow-other-keys) env
    (let* ((js (slurp-string-body raw-body ))
	   (registration-form (json-to-clos js 'register-form)))
      (handler-case 
	  (with-db
	      (with-transaction ()
		(with-slots (username displayname password password-again) registration-form
		  (assert (string= password password-again))
		  (execute "INSERT INTO pichunter.users(username, password, display_name) VALUES ($1, $2, $3)"
			   username
			   (sha-512 password)
			   displayname)
		  `(200 (:content-type "application/json"
			 :charset "utf-8")
			(,(with-output-to-string (s)
			    (encode registration-form s)))))))
	(error (e)
	  (format t "Error in \"/register\": ~a~%" e)
	  (break "~a" e)
	  
	  `(500 nil ("Internal Server Error")))))))

(defroute "get" "/api/users"
    env
  (with-db
    `(200 (:content-type "text/plain; charset=utf-8")
	  (,(str:join "\n"
		      (mapcar #'prin1-to-string
			      (query "SELECT * FROM pichunter.users")))))))
