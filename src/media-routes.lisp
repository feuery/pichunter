(defpackage pichunter.media-routes
  (:use :cl :postmodern :pichunter.decorators :pichunter.std :binding-arrows :com.inuoe.jzon)
  (:import-from :pichunter.user :user-id)
  (:import-from :easy-routes :defroute)
  (:import-from :halisql :defqueries)
  (:import-from :pichunter.decorators :@can?))

(in-package :pichunter.media-routes)

(defqueries "media-routes.sql")

(defun append-approver (image)
  (let ((approver (gethash "approver" image)))
    (if (not (equalp approver
		:null))	  
	(let* ((approver-user (mapcar #'pichunter.user-routes:fix-user-abilities
				      (coerce 
				       (pichunter.user-routes:data-for-frontend-without-password approver) 'list)))
	       (approver
		 (when (first approver-user)
		   (first approver-user))))
	  (when approver
	    (setf (gethash "approver" image)
		  approver)))

      ;; to make json-stringifier not dump "NULL" strings to the response
      (setf (gethash "approver" image) 'null))
    image))

(defroute mediamanager-data ("/api/pictures" :method :get :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
  (let ((data (or (mapcar #'append-approver (coerce (api-pictures) 'list)) #())))
    (stringify data )))

(defroute delete-picture ("/api/pictures/:guid" :method :delete :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
  (execute (:delete-from 'pictures :where (:= :id guid)))
  "true")

(defroute approve-img-really ("/api/pictures/approve/:id" :method :post :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
  (approve-image (user-id *user*) id)
  (setf (hunchentoot:return-code*) 204)
  nil)

(defroute unapprove-img ("/api/pictures/unapprove/:id" :method :post :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
  (unapprove-image id)
  (setf (hunchentoot:return-code*) 204)
  nil)
						
(defroute count-per-county ("/api/pictures/count-per-county" :method :get :decorators (@json @transaction )) ()
  (stringify
   (query
    "SELECT county_code, count(*)
FROM pictures
WHERE approver IS NOT NULL
group by county_code" :array-hash)))

(defroute unapproved-imgs ("/api/pictures/unapproved" :method :get :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
  (let ((imgs (mapcar #'append-approver (coerce (unapproved-images) 'list))))

    (stringify imgs)))
