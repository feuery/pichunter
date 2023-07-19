(defpackage :pichunter.game-routes
  (:use :cl :postmodern :pichunter.std :com.inuoe.jzon :binding-arrows :com.inuoe.jzon :pichunter.decorators)
  (:import-from :easy-routes :defroute))

(in-package :pichunter.game-routes)

(defroute next-image ("/api/next-picture" :method :get :decorators (@json @transaction @no-cache)) ()
  (stringify
   (aref 
    (query "select id, filename, latitude, longitude from pichunter.pictures order by random() limit 1" :array-hash)
    0)))

(defun load-counties ()
  (execute (:insert-rows-into 'pichunter.county
	    :columns 'code 'name

	    :values (mapcar (lambda (code)
			      (list (parse-integer (gethash "code" code))
				    (->> (coerce (gethash "classificationItemNames" code) 'list)
				      first
				      (gethash "name"))))
			    (coerce 
			     (parse (slurp-utf-8 "counties.json")
				    :allow-comments t)
			     'list)))))

;; curl https://www2.tilastokeskus.fi/fi/luokitukset/corrmaps/export/kunta_1_20160101%23maakunta_1_20160101/ -o maakunnat_kunnat.csv
(defun load-municipalities ()
  (execute (:insert-rows-into 'pichunter.municipality
	    :columns 'code 'county-code
	    :values
	    (->> (slurp #P"/home/feuer/projects/pichunter/src/maakunnat_kunnat.csv" :external-format :cp1252)
	      (str:split #\Newline)
	      (drop 5)
	      (mapcar (lambda (row)
			(let* ((splitted (str:split ";" row))
			       (municipality-code (first splitted))
			       (county-code (third splitted))
			       (data (mapcar (lambda (c)
							      (str:replace-all "'" ""
									       (str:replace-all "\"" "" c)))
					     (list municipality-code county-code))))
			  (if (second data)
			      (mapcar #'parse-integer data)))))
	      (remove-if-not #'identity )))))
  
  

(defun load-codesets ()
  (with-db 
    (with-transaction ()
      (load-counties)
      (load-municipalities))))
  
;; (load-codesets)
