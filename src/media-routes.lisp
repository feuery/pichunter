(defpackage pichunter.media-routes
  (:use :cl :postmodern :pichunter.decorators :pichunter.std :binding-arrows :com.inuoe.jzon)
  (:import-from :easy-routes :defroute)
  (:import-from :pichunter.decorators :@can?))

(in-package :pichunter.media-routes)

(defroute mediamanager-data ("/api/pictures" :method :get :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
  (stringify
   (query (:select 'id 'filename 'latitude 'longitude :from 'pictures) :array-hash)))

(defroute delete-picture ("/api/pictures/:guid" :method :delete :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
  (execute (:delete-from 'pictures :where (:= :id guid)))
  "true")
						
(defroute count-per-county ("/api/pictures/count-per-county" :method :get :decorators (@json @transaction )) ()
  (stringify
   (query
    "SELECT county_code, count(*)
FROM pictures
group by county_code" :array-hash)))
