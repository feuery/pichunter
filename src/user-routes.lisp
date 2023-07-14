(defpackage pichunter.user-routes
  (:use :cl :postmodern :pichunter.std :com.inuoe.jzon :binding-arrows)
  (:import-from :easy-routes :defroute)
  (:import-from :pichunter.decorators :@json :@transaction)
  (:import-from :pichunter.std :sha-512))

(in-package :pichunter.user-routes)

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

(defun setup-admin-user ()
  (execute "INSERT INTO pichunter.groupmapping (UserID, GroupID)
SELECT \"user\".ID, \"group\".ID
FROM pichunter.user \"user\"
JOIN pichunter.usergroup \"group\" ON 1=1;")
  (execute 
"-- at this point, there is only one row in the pichunter.user
UPDATE pichunter.user
SET activated = true;"))

(defroute register-route
    ("/api/login/register" :method :post :decorators (@transaction @json)) ()

  (let* ((body-param (parse (hunchentoot:raw-post-data :force-text t)))
	 (username (gethash "username" body-param))
	 (displayname (gethash "displayname" body-param))
	 (password (gethash "password" body-param))
	 (password-again (gethash "password-again" body-param)))
      (handler-case 
	  (let ((prior-users (query "SELECT EXISTS (SELECT * FROM pichunter.user)" :single)))
	    (assert (string= password password-again))
	    (execute "INSERT INTO pichunter.user(username, password, display_name) VALUES ($1, $2, $3)"
		     username
		     (sha-512 password)
		     displayname)
	    (unless prior-users
		      (setup-admin-user))

	    (format t "Registered ~a (~a)~%" displayname username)
	    "{\"success?\": true}")
	(error (e)
	  (format t "Error in \"/register\": ~a~%" e)
	  (break "~a" e)
	  
	  (easy-routes:http-error 500)))))

;; wtf why does this route exist?
(defroute users-route ("/api/users" :method :get :decorators (@transaction)) ()
  (setf (hunchentoot:content-type*) "application7json; charset=utf-8")
  (stringify 
  (query "SELECT * FROM pichunter.user" :array-hash)))

(defun clean-postmodern-rubbish-json (json)
  (str:replace-all "\"null\"" "null"
		   (str:replace-all "]\"" "]"
				    (str:replace-all "\"[" "["
						     (str:replace-all "\\\"" "\""
								      json)))))

(defroute post-login ("/api/login" :method :post :decorators (@transaction @json)) ()
  (let* ((body-params (parse (hunchentoot:raw-post-data :force-text t)))
	 (username (gethash "username" body-params))
	 (password (gethash "password" body-params))

	 (user-row  (query "SELECT id, username, display_name, img_id FROM pichunter.user WHERE username = $1 AND password = $2" username (sha-512 password)
			   (:dao user :single)))
	 (user-json (clean-postmodern-rubbish-json
		     (query "SELECT \"user\".id, \"user\".username, \"user\".display_name, \"user\".img_id, json_agg(\"abilities\".action) as abilities, activated as \"activated?\"
FROM pichunter.user \"user\"
JOIN pichunter.user_abilities \"abilities\" ON \"abilities\".id = \"user\".id
WHERE \"user\".username = $1 AND \"user\".password = $2
GROUP BY \"user\".id" username (sha-512 password)
:json-str))))

    (if (and user-row
	     (string= (user-username user-row) username))
	(progn
	  (setf (hunchentoot:session-value :logged-in-username) username)
	  (setf (hunchentoot:session-value :logged-in-user-id) (user-id user-row))
	  user-json)

	(progn 
	  (setf (hunchentoot:return-code*) 401)
	  "not authorized"))))

(defroute getsession ("/api/session" :method :get :decorators (@transaction @json)) ()
  (if (hunchentoot:session-value :logged-in-username)
      (let ((user (clean-postmodern-rubbish-json
		   (query "SELECT \"user\".id, \"user\".username, \"user\".display_name, \"user\".img_id, json_agg(\"abilities\".action) as abilities, activated as \"activated?\"
FROM pichunter.user \"user\"
JOIN pichunter.user_abilities \"abilities\" ON \"abilities\".id = \"user\".id
WHERE \"user\".id = $1
GROUP BY \"user\".id" (hunchentoot:session-value :logged-in-user-id)
:json-str))))
	user)
      (progn
	(setf (hunchentoot:return-code*) 401)
	"not authorized")))

(defroute do-logout ("/api/logout" :method :get) ()
  (setf (hunchentoot:session-value :logged-in-username) nil)
  (setf (hunchentoot:session-value :logged-in-user-id) nil)
  (setf (hunchentoot:return-code*) 204)
  "success")

(defun user-plist->user-response (user)
  (let ((img-id (getf user :imgId))
	(hashmap (make-hash-table :test 'equal :size 6)))
    (setf (gethash "abilities" hashmap) '(""))
    (setf (gethash "imgId" hashmap) (if (equalp img-id :null)
					""
					img-id))
    (setf (gethash "username" hashmap) (getf user :username))
    (setf (gethash "displayName" hashmap) (getf user :displayName ))
    (setf (gethash "id" hashmap) (getf user :user-id))
    (setf (gethash "activated?" hashmap) (getf user :activated))
    hashmap))

(defun save-user (group-id user)
  (let ((id (gethash "id" user))
	(displayName (gethash "displayName" user))
	(activated? (gethash "activated?" user)))
    (execute (:update 'pichunter.user
	      :set 'display_name displayName
	      'activated activated?
	      :where (:= 'id id)))
    (execute (:insert-into 'pichunter.groupmapping
	      :set 'UserID id 'GroupID group-id))))

(defun save-permission (group-id permission)
  (execute (:insert-into 'pichunter.GroupPermission
	    :set 'PermissionID (gethash "id" permission)
	         'GroupID group-id)))

(defun save-group (group)
  (let ((id (gethash "id" group))
	(name (gethash "name" group))
	(users (remove-duplicates
		(coerce (gethash "users" group) 'list)
		:test (lambda (a b)
			      (equalp (gethash "id" a)
				      (gethash "id" b)))))
	(description (gethash "description" group))
	(permissions (remove-duplicates
		      (->> (coerce (gethash "permissions" group) 'list)
		       (remove-if (lambda (perm)
				    (equalp 'NULL
					    (gethash "id" perm)))))
		      :test (lambda (a b)
			      (equalp (gethash "id" a)
				      (gethash "id" b))))))
    (execute (:update 'pichunter.usergroup
	      :set 'name name
	           'description description
		   :where (:= 'id id)))
    (execute (:delete-from 'pichunter.groupmapping :where (:= 'GroupID id)))
    (execute (:delete-from 'pichunter.GroupPermission :where (:= 'GroupID id)))
    (dolist (user users)
      (save-user id user))
    (dolist (permission permissions)
	(save-permission id permission))))
	      

(defroute grouptree-saver ("/api/grouptree" :method :post :decorators (@transaction @json)) ()
  (let ((body-params (parse (hunchentoot:raw-post-data :force-text t))))
    (dolist (group (coerce body-params 'list))
      (save-group group))
    "{\"success\": true}"))

(defun transform-permission (row)
  (let ((hashmap (make-hash-table :test 'equal :size 2)))
    
    (setf (gethash "action" hashmap) (getf row :action))
    (setf (gethash "id" hashmap) (getf row :permission-id))
    hashmap))

(defroute grouptree-getter ("/api/grouptree" :method :get :decorators (@json @transaction)) ()
    (let* ((groups (query (:select (:as :usergroup.id :group-id)
				   (:as :usergroup.name :group-name)
				   (:as :usergroup.description :group-description)
				   (:as :permission.id :permission-id)
				   (:as :permission.action :action)
				   (:as :user.id :user-id)
				   (:as :user.username :username)
				   (:as :user.activated :activated)
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
	   (all-abilities (->> (query (:select :action (:as :id :permission-id)
					       :from :pichunter.permission)
				      :plists)
			    (mapcar #'transform-permission)))
	   (all-users (->> (query (:select (:as :user.id :user-id)
				      (:as :user.activated :activated)
				      (:as :user.username :username)
				      (:as :display-name :displayName)
				      (:as :img_id :imgId)
				      :from :pichunter.user)
				  :plists)
			;; (mapcar (lambda (row)
			;; 	  (let ((hashmap (make-hash-table :test 'equal :size 5)))
			;; 	    (setf (gethash "user-id" hashmap) (getf row :user-id))
			;; 	    (setf (gethash "activated" hashmap) (getf row :activated))
			;; 	    (setf (gethash "username" hashmap) (getf row :username))
			;; 	    (setf (gethash "displayName" hashmap) (getf row :displayName))
			;; 	    (setf (gethash "imgId" hashmap) (getf row :imgId))
			;; 	    hashmap)))
			))
	   (actual-groups (remove-duplicates
			   (mapcar (lambda (g)
				     (let ((hashmap (make-hash-table :test 'equal :size 7)))
				       (setf (gethash "all-users" hashmap) (mapcar #'user-plist->user-response all-users))
				       (setf (gethash "all-abilities" hashmap) all-abilities)
				       (setf (gethash "description" hashmap) (getf g :group-description))
				       (setf (gethash "name" hashmap) (getf g :group-name))
				       (setf (gethash "id" hashmap) (getf g :group-id))
				       (setf (gethash "permissions" hashmap) (remove-duplicates
									      (->>
										groups
										(remove-if
										       (lambda (r)
											 (equalp
											  (getf r :permission-id)
											  :NULL)))
										(remove-if-not 
										 (lambda (r)
										   (equalp
										    (getf r :group-id)
										    (getf g :group-id))))
										(mapcar #'transform-permission))
									      :test (lambda (a b)
										      (equalp
										       (gethash "id" a)
										       (gethash "id" b)))))
				       (setf (gethash "users" hashmap) (remove-duplicates
									(->>
									  groups
									  (remove-if-not 
										 (lambda (r)
										   (equalp
										    (getf r :group-id)
										    (getf g :group-id))))
									  (mapcar #'user-plist->user-response))
									
									:test (lambda (a b)
										(equalp
										 (gethash "id" a)
										 (gethash "id" b)))))
				       hashmap))
				   groups)
			   :test (lambda (a b)
				   (equalp
				    (gethash "id" a)
				    (gethash "id" b))))))

      (stringify actual-groups)))
