(defpackage pichunter.user-routes
  (:use :cl :postmodern :pichunter.std :com.inuoe.jzon :binding-arrows :pichunter.user)
  (:import-from :pichunter.user :user-id)
  (:import-from :easy-routes :defroute)
  (:import-from :pichunter.decorators :@json :@transaction)
  (:import-from :pichunter.std :sha-512)
  (:export :user :user-username :register :post-login))

(in-package :pichunter.user-routes)

(defun setup-admin-user ()
  (execute "INSERT INTO groupmapping (UserID, GroupID)
SELECT \"user\".ID, \"group\".ID
FROM users \"user\"
JOIN usergroup \"group\" ON 1=1;")
  (execute 
"-- at this point, there is only one row in the user
UPDATE users
SET activated = true;"))

(defun register (username displayname password)
  (let ((prior-users (query "SELECT EXISTS (SELECT * FROM users)" :single)))
    (execute "INSERT INTO users(username, password, display_name) VALUES ($1, $2, $3)"
	     username
	     (sha-512 password)
	     displayname)
    (unless prior-users
      (setup-admin-user))))

(defroute register-route
    ("/api/login/register" :method :post :decorators (@transaction @json)) ()

  (let* ((body-param (parse (hunchentoot:raw-post-data :force-text t)))
	 (username (gethash "username" body-param))
	 (displayname (gethash "displayname" body-param))
	 (password (gethash "password" body-param))
	 (password-again (gethash "password-again" body-param)))
    (handler-case
	(progn
	  (assert (string= password password-again))

	  (register username displayname password)

	  (format t "Registered ~a (~a)~%" displayname username)
	  "{\"success?\": true}")
      (error (e)
	(format t "Error in \"/register\": ~a~%" e)
	(break "~a" e)
	
	(easy-routes:http-error 500)))))

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

	 (user-row  (query "SELECT id, username, display_name, img_id FROM users WHERE username = $1 AND password = $2" username (sha-512 password)
			   (:dao user :single)))
	 (data-for-frontend (query "SELECT \"user\".id, \"user\".username, \"user\".display_name AS \"displayName\", \"user\".img_id AS \"imgId\", json_agg(\"abilities\".action) as abilities, activated as \"activated?\"
FROM users \"user\"
JOIN user_abilities \"abilities\" ON \"abilities\".id = \"user\".id
WHERE \"user\".username = $1 AND \"user\".password = $2
GROUP BY \"user\".id" username (sha-512 password) :array-hash)))
      
    (if (and user-row
	     (string= (user-username user-row) username))
	(progn
	  (setf data-for-frontend (aref data-for-frontend 0))
	  (setf (gethash "abilities" data-for-frontend) (remove-duplicates (coerce
								      (parse (gethash "abilities" data-for-frontend))
								      'list)
									   :test 'equal))
	  ;; (when (or (equalp (gethash "imgId" data-for-frontend)
	  ;; 		    "NULL")
	  ;; 	    (equalp (gethash "imgId" data-for-frontend)
	  ;; 		    :NULL))
	  ;;   (remhash "imgId" data-for-frontend))
	  ;; (break)
	  (setf (hunchentoot:session-value :logged-in-username) username)
	  (setf (hunchentoot:session-value :logged-in-user-id) (user-id user-row))
	  (stringify data-for-frontend))

	(progn 
	  (setf (hunchentoot:return-code*) 401)
	  "not authorized"))))

(defroute getsession ("/api/session" :method :get :decorators (@transaction @json)) ()
  (if (hunchentoot:session-value :logged-in-username)
      (let ((users (coerce (query "SELECT \"user\".id, \"user\".username, \"user\".display_name AS \"displayName\", \"user\".img_id AS \"imgId\", json_agg(\"abilities\".action) as abilities, activated as \"activated?\"
FROM users \"user\"
JOIN user_abilities \"abilities\" ON \"abilities\".id = \"user\".id
WHERE \"user\".id = $1
GROUP BY \"user\".id" (hunchentoot:session-value :logged-in-user-id) :array-hash)
			  'list)))
	(if users
	    (let ((user (first users)))
	      (setf (gethash "abilities" user) (remove-duplicates (coerce
								   (parse (gethash "abilities" user))
								   'list)
								  :test 'equal))	      
	      (stringify user))
	    (progn
	      (setf (hunchentoot:return-code*) 500)
	      "server error")))
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
    (execute (:update 'users
	      :set 'display_name displayName
	      'activated activated?
	      :where (:= 'id id)))
    (execute (:insert-into 'groupmapping
	      :set 'UserID id 'GroupID group-id))))

(defun save-permission (group-id permission)
  (execute (:insert-into 'GroupPermission
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
    (execute (:update 'usergroup
	      :set 'name name
	           'description description
		   :where (:= 'id id)))
    (execute (:delete-from 'groupmapping :where (:= 'GroupID id)))
    (execute (:delete-from 'GroupPermission :where (:= 'GroupID id)))
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
				   (:as :users.id :user-id)
				   (:as :users.username :username)
				   (:as :users.activated :activated)
				   (:as :display-name :displayName)
				   (:as :img_id :imgId)
				   (:as "[]" :abilities)
				   :from :usergroup
				   
				   :join :groupmapping 
				   :on (:= :groupmapping.groupid :usergroup.id)
				   
				   :join :users
				   :on (:= :groupmapping.userid :users.id)

				   :left-join :grouppermission
				   :on (:= :grouppermission.groupid :usergroup.id)

				   :left-join :permission
				   :on (:= :grouppermission.permissionid :permission.id))
			  :plists))
	   (all-abilities (->> (query (:select :action (:as :id :permission-id)
					       :from :permission)
				      :plists)
			    (mapcar #'transform-permission)))
	   (all-users (->> (query (:select (:as :users.id :user-id)
				      (:as :users.activated :activated)
				      (:as :users.username :username)
				      (:as :display-name :displayName)
				      (:as :img_id :imgId)
				      :from :users)
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
				       (setf (gethash "permissions" hashmap)
					     (or (remove-duplicates
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
										       (gethash "id" b))))
						 (vector)))
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
