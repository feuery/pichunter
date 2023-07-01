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

(defun setup-admin-user ()
  (execute "INSERT INTO pichunter.groupmapping (UserID, GroupID)
SELECT \"user\".ID, \"group\".ID
FROM pichunter.user \"user\"
JOIN pichunter.usergroup \"group\" ON 1=1;"))

(defroute "post" "/register"
    env  
  (destructuring-bind (&key raw-body &allow-other-keys) env
    (let* ((js (slurp-string-body raw-body ))
	   (registration-form (json-to-clos js 'register-form)))
      (handler-case 
	  (with-db
	      (with-transaction ()
		(let ((prior-users (query "SELECT EXISTS (SELECT * FROM pichunter.user)" :single)))
		  (with-slots (username displayname password password-again) registration-form
		    (assert (string= password password-again))
		    (execute "INSERT INTO pichunter.user(username, password, display_name) VALUES ($1, $2, $3)"
			     username
			     (sha-512 password)
			     displayname)
		    (unless prior-users
		      (setup-admin-user))
		    `(200 (:content-type "application/json"
			   :charset "utf-8")
			  (,(with-output-to-string (s)
			      (encode registration-form s))))))))
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
			      (query "SELECT * FROM pichunter.user")))))))

;; I hate the fact that combining the postmodern-dao-metaclass-thing and json-serialization-metaclass seems impossible
(defun clean-postmodern-rubbish-json (json)
  (str:replace-all "\"null\"" "null"
		   (str:replace-all "]\"" "]"
				    (str:replace-all "\"[" "["
						     (str:replace-all "\\\"" "\""
								      json)))))

(defroute "post" "/api/login"
    env
  (destructuring-bind (&key raw-body &allow-other-keys) env
    (let* ((session (getf env :lack.session))
	   (params (json-to-clos (slurp-string-body raw-body) 'login-request)))
      (with-slots (username password) params
	(with-db
	    (let ((user-row  (query "SELECT id, username, display_name, img_id FROM pichunter.user WHERE username = $1 AND password = $2" username (sha-512 password)
				    (:dao user :single)))
		  (user-json (clean-postmodern-rubbish-json
			      (query "SELECT \"user\".id, \"user\".username, \"user\".display_name, \"user\".img_id, json_agg(\"abilities\".action) as abilities
FROM pichunter.user \"user\"
JOIN pichunter.user_abilities \"abilities\" ON \"abilities\".id = \"user\".id
WHERE \"user\".username = $1 AND \"user\".password = $2
GROUP BY \"user\".id" username (sha-512 password)
:json-str))))

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
	    (let ((user (clean-postmodern-rubbish-json
			 (query "SELECT \"user\".id, \"user\".username, \"user\".display_name, \"user\".img_id, json_agg(\"abilities\".action) as abilities
FROM pichunter.user \"user\"
JOIN pichunter.user_abilities \"abilities\" ON \"abilities\".id = \"user\".id
WHERE \"user\".id = $1
GROUP BY \"user\".id" (gethash :logged-in-user-id session)
:json-str))))
	      `(200 nil (,user))))
	`(401 nil nil))))

(defroute "get" "/api/logout"
    env
  (let ((session (getf env :lack.session)))
    (remhash :logged-in-username session)
    (remhash :logged-in-user-id session)
    `(204 nil nil)))

(defclass permission-response ()
  ((id :accessor permission-id :initarg :id :json-type :any :json-key "id")
   (action :accessor permission-action :initarg :action :json-type :any :json-key "action"))
  (:metaclass json-serializable-class))

(defclass user-response (login-request)
  ((username
    :accessor username
    :initarg :username
    :json-type :string
    :json-key "username")
   (displayname
    :accessor displayname
    :initarg :displayname
    :json-type :string
    :json-key "displayName")
   (id
    :accessor user-id
    :initarg :id
    :json-type :number
    :json-key "id")
   (abilities :accessor abilities
	      :initarg :abilities
	      :json-type :any
	      :json-key "abilities")
   (imgId :accessor imgId
	  :initarg :imgId
	  :json-type :string
	  :json-key "imgId"))
  (:metaclass json-serializable-class))

(defclass group-response ()
  ((id :accessor group-id :initarg :id :json-type :number :json-key "id")
   (name :accessor group-name :initarg :name :json-type :string :json-key "name")
   (description :accessor group-description :initarg :description :json-type :string :json-key "description")
   (permissions :accessor group-permissions :initarg :permissions :json-type (:list permission-response) :json-key "permissions")
   (all-abilities :accessor all-abilities :initarg :all-abilities :json-type :any :json-key "all-abilities")
   (users :accessor group-users :initarg :users :json-type (:list user-response) :json-key "users"))
  (:metaclass json-serializable-class))

;; FIXME add authentication
(defroute "get" "/api/grouptree"
    env
  (with-db
      (with-transaction ()
	(let* ((groups (query (:select (:as :usergroup.id :group-id)
				       (:as :usergroup.name :group-name)
				       (:as :usergroup.description :group-description)
				       (:as :permission.id :permission-id)
				       (:as :permission.action :action)
				       (:as :user.id :user-id)
				       (:as :user.username :username)
				       (:as :display-name :displayName)
				       (:as :img_id :imgId)
				       (:as "[]" :abilities)
				       :from :pichunter.usergroup
				       
				       :join :pichunter.groupmapping 
				       :on (:= :groupmapping.groupid :usergroup.id)
				       
				       :join :pichunter.user
				       :on (:= :groupmapping.userid :user.id)

				       :left-join :pichunter.grouppermission
				       :on (:= :grouppermission.groupid :usergroup.id)

				       :left-join :pichunter.permission
				       :on (:= :grouppermission.permissionid :permission.id))
			      :plists))
	       (all-abilities (query (:select :action :id
					      :from :pichunter.permission)
				     :plists))
	       (actual-groups (remove-duplicates
			       (mapcar (lambda (g)
					 (make-instance 'group-response
							:all-abilities (mapcar (lambda (permission)
										(make-instance 'permission-response
											       :action (getf permission :action )
											       :id (getf permission :id)))
									       all-abilities)
							:description (getf g :group-description)
							:name (getf g :group-name)
							:id (getf g :group-id)
							:permissions (remove-duplicates
								      (mapcar (lambda (permission)
										(make-instance 'permission-response
											       :action (getf permission :action )
											       :id (getf permission :permission-id)))
									      groups)
								      :test (lambda (a b)
									      (equalp
									       (permission-id a)
									       (permission-id b))))
							:users (remove-duplicates
								(mapcar (lambda (user)
									  (let* ((img-id (getf user :imgId)))
									    (make-instance 'user-response
											   :abilities '("")
											   :imgId (if (equalp img-id :null)
												      ""
												      img-id)
											   :username (getf user :username)
											   :displayname (getf user :displayName )
											   :id (getf user :user-id))))
									groups)
								:test (lambda (a b)
									(equalp
									 (user-id a)
									 (user-id b))))))
				       groups)
					 :test (lambda (a b)
						 (equalp (group-id a)
							 (group-id b))))))

	  `(200 (:content-type "application/json"
		 :charset "utf-8")
		(,(with-output-to-string (s)
		    (encode  actual-groups
			    s))))))))
