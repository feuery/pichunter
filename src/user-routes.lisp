(defpackage pichunter.user-routes
  (:use :cl :postmodern :pichunter.std :com.inuoe.jzon :binding-arrows :pichunter.user)
  (:import-from :pichunter.user :user-id)
  (:import-from :easy-routes :defroute)
  (:import-from :pichunter.decorators :*user* :@json :@transaction :@authenticated :@can?)
  (:import-from :pichunter.std :sha-512)

  (:import-from :halisql :defqueries)
  
  (:export :data-for-frontend-without-password :get-highest-scores-per-session :user :user-username :register :post-login :fix-user-abilities))

(in-package :pichunter.user-routes)

(defqueries "user-routes.sql") 

(defun setup-admin-user ()
  (make-everybody-member-of-everything!)
  (activate-everybody!))

(defun register (username displayname password)
  (let ((prior-users (users-exists?)))
    (insert-user
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
	  
	  "{\"success?\": true}")
      (error (e)
	;; TODO oispa lokitusta
	(format t "Error in \"/register\": ~a~%" e)	
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
	 (user-row (select-user-by-login username (sha-512 password)))
	 (data-for-frontend (data-for-frontend-with-password username (sha-512 password))))
      
    (if (and user-row
	     (string= (user-username user-row) username))
	(progn
	  (setf data-for-frontend (aref data-for-frontend 0))
	  (setf (gethash "abilities" data-for-frontend) (remove-duplicates (coerce
								      (parse (gethash "abilities" data-for-frontend))
								      'list)
									   :test 'equal))
	  
	  (setf (hunchentoot:session-value :logged-in-username) username)
	  (setf (hunchentoot:session-value :logged-in-user-id) (user-id user-row))
	  (stringify data-for-frontend))

	(progn 
	  (setf (hunchentoot:return-code*) 401)
	  "not authorized"))))

(defun fix-user-abilities (user)
  (let ((abilities (coerce (parse (gethash "abilities" user)) 'list)))
    (setf (gethash "abilities" user)
	  (remove-duplicates abilities :test 'equal))
    user))

(defroute getsession ("/api/session" :method :get :decorators (@transaction @json)) ()
  (if (hunchentoot:session-value :logged-in-username)
      (let ((users (mapcar #'fix-user-abilities
			   (coerce (data-for-frontend-without-password (hunchentoot:session-value :logged-in-user-id))
			  'list))))
	(if users
	    (let ((user (first users)))	      
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
    (setf (gethash "banned?" hashmap) (getf user :banned))
    hashmap))

(defun save-user (group-id user)
  (let ((id (gethash "id" user))
	(displayName (gethash "displayName" user))
	(activated? (gethash "activated?" user))
	(banned? (gethash "banned?" user)))
    (execute (:update 'users
	      :set 'display_name displayName
	      'activated activated?
	      'banned banned?
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
	      

(defroute grouptree-saver ("/api/grouptree" :method :post :decorators (@transaction @json @authenticated (@can? "can-admin"))) ()
  (let ((body-params (parse (hunchentoot:raw-post-data :force-text t))))
    (dolist (group (coerce body-params 'list))
      (save-group group))
    "{\"success\": true}"))

(defun transform-permission (row)
  (let ((hashmap (make-hash-table :test 'equal :size 2)))
    
    (setf (gethash "action" hashmap) (getf row :action))
    (setf (gethash "id" hashmap) (getf row :permission-id))
    hashmap))

(defroute grouptree-getter ("/api/grouptree" :method :get :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
    (let* ((groups (query (:select (:as :usergroup.id :group-id)
				   (:as :usergroup.name :group-name)
				   (:as :usergroup.description :group-description)
				   (:as :permission.id :permission-id)
				   (:as :permission.action :action)
				   (:as :users.id :user-id)
				   (:as :users.username :username)
				   (:as :users.activated :activated)
			           (:as :users.banned :banned)
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
				      (:as :users.banned :banned)
				      (:as :users.username :username)
				      (:as :display-name :displayName)
				      (:as :img_id :imgId)
				      :from :users)
				  :plists)))
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

(defun hash->user (hash)
  (let ((user (user-by-id (gethash "id" hash))))
    (setf (user-username user) (gethash "username" hash))
    (setf (user-display-name user) (gethash "displayName" hash))
    user))

(defroute usersettings-saver ("/api/user" :method :post :decorators (@json @transaction @authenticated)) (&post file user new_password old_password)

  (let ((user (hash->user (parse user))))

    (when (equalp (user-id user)
		  (user-id *user*))

      (cond ((string= new_password "")
	     (update-names	      
	       (user-username user)
	       (user-display-name user)
	       (user-id user)))

	    ((string=
	      (caar (get-password-by-id  (user-id *user*)))
	      (sha-512 old_password))

	     (update-everything
	      (user-username user)
	      (user-display-name user)
	      new_password
	      (user-id user)
	      (sha-512 old_password))))

      (when file
	(destructuring-bind (tmp-file filename mime) file
	  (let* ((bytes (slurp-bytes tmp-file))
		 (avatar-id (caar (insert-avatar filename mime bytes))))

	    (update-avatar-reference
	     avatar-id
	     (user-id user))))))
    ""))

(defun get-highest-scores-per-session (user)
  (let ((query-result
	  (coerce
	   (get-session-scores (user-id user)) 'list))
	(response (make-hash-table :test 'equal :size 2)))
    (dolist (row query-result)
      (let ((node (make-hash-table :test 'equal :size 2)))
	(setf (gethash "correct_guesses" node) (gethash "correct" row))
	(setf (gethash "all_guesses" node) (gethash "all_guesses" row))
	
	(setf (gethash (gethash "gametype" row) response) node)))

    (unless (gethash "picture" response)
      (setf (gethash "picture" response) nil))

    (unless (gethash "location" response)
      (setf (gethash "location" response) nil))
    
    response))

(defroute get-highest-sessions ("/api/session/highest" :method :get :decorators (@json @transaction @authenticated)) ()
  (stringify (get-highest-scores-per-session *user*)))

    
