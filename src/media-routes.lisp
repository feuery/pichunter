(defpackage pichunter.media-routes
  (:use :cl :postmodern :pichunter.std :com.inuoe.jzon :binding-arrows :com.inuoe.jzon)
  (:import-from :easy-routes :defroute)
  (:import-from :pichunter.decorators :@json :@transaction))

(in-package :pichunter.media-routes)

(defroute mediamanager-data ("/api/pictures" :method :get :decorators (@json @transaction)) ()

  ;; (mapcar (lambda (r)
  ;; 	    (let ((hashmap (make-hash-table :size 2 :test 'equal)))
  ;; 	      (setf (gethash "id" hashmap)
  ;; 		    (getf r :id))
  ;; 	      (setf (gethash "filename" hashmap)
  ;; 		    (getf
  (stringify
   (query (:select 'id 'filename 'latitude 'longitude :from 'pichunter.pictures) :array-hash)))

(defroute delete-picture ("/api/pictures/:guid" :method :delete :decorators (@json @transaction)) ()
  (execute (:delete-from 'pichunter.pictures :where (:= :id guid)))
  "true")
						

    
