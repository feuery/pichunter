(defpackage pichunter.user-routes
  (:use :cl :postmodern :json-mop :pichunter.std)
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

(defclass login-request ()
  ((username :initarg :username
	     :json-type :string
	     :json-key "username")
   (password :initarg :password
	     :json-type :string
	     :json-key "password"))
  (:metaclass json-serializable-class))

(defclass user ()
  ((username :initarg username


	     :col-type string
	     :accessor user-username)
   (id :initarg id
       :col-type integer
       :accessor user-id)
   (display_name :initarg display-name
		 :col-type string
		 :accessor user-display-name)
   (img_id :initarg image-id
	   :col-type string
	   :accessor user-image-id))
  (:metaclass dao-class)
  (:keys id))

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

(defroute "post" "/api/login"
    env
  (destructuring-bind (&key raw-body &allow-other-keys) env
    (let* ((session (getf env :lack.session))
	   (params (json-to-clos (slurp-string-body raw-body) 'login-request)))
      (with-slots (username password) params
	(with-db
	  
	    (let ((user-row  (query "SELECT id, username, display_name, img_id FROM pichunter.users WHERE username = $1 AND password = $2" username (sha-512 password)
				    (:dao user :single)))
		  (user-json (query "SELECT id, username, display_name, img_id FROM pichunter.users WHERE username = $1 AND password = $2" username (sha-512 password)
				    :json-str)))


	      (if (and user-row
		       (string= (user-username user-row) username))
		  (progn
		    (setf (gethash :logged-in-username session) username)
		    (setf (gethash :logged-in-user-id session) (user-id user-row))

		    `(200 nil (,user-json)))
		  `(401 nil nil))))))))

(defroute "get" "/api/session"
    env
  (let ((session (getf env :lack.session)))
    (if (gethash :logged-in-username session)
	(with-db 
	    (let ((user (query "SELECT id, username, display_name, img_id FROM pichunter.users WHERE id = $1" (gethash :logged-in-user-id session)
			       :json-str)))
	      `(200 nil (,user))))
	`(401 nil nil))))

(defroute "get" "/api/logout"
    env
  (let ((session (getf env :lack.session)))
    (remhash :logged-in-username session)
    (remhash :logged-in-user-id session)
    `(204 nil nil)))
