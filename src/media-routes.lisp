(defpackage pichunter.media-routes
  (:use :cl :postmodern :pichunter.decorators :pichunter.std :binding-arrows :com.inuoe.jzon)
  (:import-from :easy-routes :defroute)
  (:import-from :halisql :defqueries)
  (:import-from :pichunter.decorators :@can?))

(in-package :pichunter.media-routes)

(defqueries "media-routes.sql")


(defroute mediamanager-data ("/api/pictures" :method :get :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
  (stringify (api-pictures))) 

(defroute delete-picture ("/api/pictures/:guid" :method :delete :decorators (@json @transaction @authenticated (@can? "can-admin"))) ()
  (execute (:delete-from 'pictures :where (:= :id guid)))
  "true")
						
(defroute count-per-county ("/api/pictures/count-per-county" :method :get :decorators (@json @transaction )) ()
  (stringify
   (query
    "SELECT county_code, count(*)
FROM pictures
group by county_code" :array-hash)))
