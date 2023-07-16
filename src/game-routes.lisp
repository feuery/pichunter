(defpackage :pichunter.game-routes
  (:use :cl :postmodern :pichunter.std :com.inuoe.jzon :binding-arrows :com.inuoe.jzon :pichunter.decorators)
  (:import-from :easy-routes :defroute))

(in-package :pichunter.game-routes)

(defroute next-image ("/api/next-picture" :method :get :decorators (@json @transaction @no-cache)) ()
  (stringify
   (aref 
    (query "select id, filename, latitude, longitude from pichunter.pictures order by random() limit 1" :array-hash)
    0)))

  
