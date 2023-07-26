(defpackage :pichunter.game-routes
  (:use :cl :postmodern :pichunter.std :com.inuoe.jzon :binding-arrows :com.inuoe.jzon :pichunter.decorators :pichunter.user)
  (:import-from :pichunter.file-handler :coordinate->number)
  (:import-from :easy-routes :defroute))

(in-package :pichunter.game-routes)

(defroute next-image ("/api/next-picture/:county-code" :method :get :decorators (@json @transaction @no-cache @authenticated)) (&get gametype)
  (let* ((id (user-id *user*))
	 (results (coerce (cond ((string= gametype "picguess")
				 (query
				  "SELECT pic.id, pic.filename, pic.latitude, pic.longitude
FROM pichunter.pictures pic
LEFT JOIN pichunter.pictureguessing_session session ON session.picture_id = pic.id
WHERE county_code = $1
AND   guessed_at IS NULL
AND   (user_id = $2 OR user_id IS NULL)
ORDER BY random()
limit 1" county-code id :array-hash))
				(t (query "select id, filename, latitude, longitude from pichunter.pictures where county_code = $1 order by random() limit 1" county-code :array-hash))) 'list )))

    (if results
	(let ((result (first results)))
	  (when (and (string= gametype "picguess")
		     (not (query "SELECT * FROM pichunter.pictureguessing_session WHERE guessed_at IS NULL AND user_id = $1" id)))
	    (execute "INSERT INTO pichunter.pictureguessing_session (user_id, picture_id) VALUES ($1, $2)" id (gethash "id" result)))	  
	  (stringify result))

	"null")))

(defun get-position (tmp-file)
  (let ((exif (handler-case
		    (zpb-exif:make-exif tmp-file)
		  (zpb-exif:invalid-jpeg-stream (e) nil)
		  (zpb-exif:invalid-exif-stream (e) nil))))
      (when exif
	(let* ((gps-data (zpb-exif:ifd-alist (zpb-exif:gps-ifd exif)))
	       (latitude (coerce (cdr (assoc "GPSLatitude" gps-data :test #'string=)) 'list))
	       (latitude-number (coordinate->number latitude))
	       (longitude (coerce (cdr (assoc "GPSLongitude" gps-data :test #'string=)) 'list))
	       (longitude-number (coordinate->number longitude)))
	  (values latitude-number longitude-number)))))

(defun distance (lat1 long1
		 lat2 long2)
  (let* ((loc1 (cl-geocode:make-location :latitude lat1 :longitude long1))
	 (loc2 (cl-geocode:make-location :latitude lat2 :longitude long2))
	 (distance (cl-geocode:distance-between loc1 loc2 :unit :kilometers)))
    (* distance 1000)))

(defroute guess-picture ("/api/guess-picture" :method :post :decorators (@json @transaction @no-cache @authenticated)) (&post file)
  (destructuring-bind (tmp-file filename mime) file
    (multiple-value-bind (input-latitude input-longitude) (get-position tmp-file)
      (with-slots (id) *user*
	(let ((asked-image (query
			    "SELECT data, pic.id
FROM pichunter.pictures pic
JOIN pichunter.pictureguessing_session session ON pic.id = session.picture_id
WHERE user_id = $1 AND guessed_at IS NULL" id :array-hash)))	  
	  (multiple-value-bind (asked-latitude asked-longitude) (get-position (flexi-streams:make-in-memory-input-stream (gethash "data" (aref asked-image 0))))
	    (let* ((d (distance input-latitude input-longitude
			       asked-latitude asked-longitude))
		   (correct? (< d 100)))

	      (when correct?
		(execute
"UPDATE pichunter.pictureguessing_session 
SET guessed_at = NOW()
WHERE user_id = $1 AND picture_id = $2" id (gethash "id" (aref asked-image 0))))
	      
	      (format nil "{\"correct?\": ~a}" (if (< d 100)
						   "true"
						   "false")))))))))
	  

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
