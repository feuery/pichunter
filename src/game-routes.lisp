(defpackage :pichunter.game-routes
  (:use :cl :postmodern :pichunter.std :com.inuoe.jzon :binding-arrows :com.inuoe.jzon :pichunter.decorators :pichunter.user)
  (:import-from :pichunter.file-handler :coordinate->number)
  (:import-from :easy-routes :defroute)
  (:export :get-position :load-codesets))

(in-package :pichunter.game-routes)

(defun init-session (user-id)

  (let ((session-id (getf (query "SELECT id FROM locationguessing_session WHERE completed_at IS NULL AND user_id = $1" user-id :plist) :id)))
    (if session-id
	session-id
	(caar (query "INSERT INTO locationguessing_session (user_id) VALUES ($1) returning id" user-id)))))
  

(defroute next-image ("/api/next-picture/:county-code" :method :get :decorators (@json @transaction @no-cache @authenticated)) (&get gametype gamesession)
  (let* ((gamesession (when (and (not (string= gametype "picguess"))
				 (not gamesession))
			(init-session (user-id *user*))))
	 (id (user-id *user*))
	 (results (coerce (cond ((string= gametype "picguess")
				 (query
				  "SELECT pic.id, pic.filename, pic.latitude, pic.longitude
FROM pictures pic
LEFT JOIN pictureguessing_session session ON session.picture_id = pic.id
WHERE county_code = $1
AND   guessed_at IS NULL
AND   (user_id = $2 OR user_id IS NULL)
ORDER BY random()
limit 1" county-code id :array-hash))
				(gamesession
				 (query "
SELECT pic.id, pic.filename, pic.latitude, pic.longitude
FROM pictures pic
JOIN locationguessing_session session ON session.user_id = $1 AND completed_at IS NULL
WHERE pic.id NOT IN (select picture_id from locationguessing_session_guess where session_id = session.id)
ORDER BY random()
LIMIT 1" id :array-hash))) 'list )))

    (if (equalp 0 (length results))
	(break))
		

    (format t "found ~d results for the next image in game type ~a ~% game session id is ~a~% the results: ~a~%" (length results) gametype gamesession results)
    
    (if results
	(let ((result (first results)))
	  (when (and (string= gametype "picguess")
		     (not (query "SELECT * FROM pictureguessing_session WHERE guessed_at IS NULL AND user_id = $1" id)))
	    (execute "INSERT INTO pictureguessing_session (user_id, picture_id) VALUES ($1, $2)" id (gethash "id" result)))

	  (setf (gethash "session-id" result) gamesession)
	  
	  (stringify result))
	"null")))

(defun get-position (tmp-file)
  (let ((exif (handler-case
		  (zpb-exif:make-exif tmp-file)
		(zpb-exif:invalid-jpeg-stream (e) nil)
		(zpb-exif:invalid-exif-stream (e) nil))))
    (when exif
      (let ((gps-data (zpb-exif:ifd-alist (zpb-exif:gps-ifd exif))))
	(when gps-data
	  (let* ((latitude (coerce (cdr (assoc "GPSLatitude" gps-data :test #'string=)) 'list))
		 (latitude-number (coordinate->number latitude))
		 (longitude (coerce (cdr (assoc "GPSLongitude" gps-data :test #'string=)) 'list))
		 (longitude-number (coordinate->number longitude)))
	    (when (and latitude-number
		       longitude-number)
	      (values latitude-number longitude-number))))))))

(defun distance (lat1 long1
		 lat2 long2)
  (let* ((loc1 (cl-geocode:make-location :latitude lat1 :longitude long1))
	 (loc2 (cl-geocode:make-location :latitude lat2 :longitude long2))
	 (distance (cl-geocode:distance-between loc1 loc2 :unit :kilometers)))
    (* distance 1000)))

(defroute guess-picture ("/api/guess-picture" :method :post :decorators (@json @transaction @no-cache @authenticated)) (&post file)
  (destructuring-bind (tmp-file filename mime) file
    (multiple-value-bind (input-latitude input-longitude) (get-position tmp-file)

      (let* ((id (user-id *user*))
	     (asked-image (query
			   "SELECT data, pic.id
FROM pictures pic
JOIN pictureguessing_session session ON pic.id = session.picture_id
WHERE user_id = $1 AND guessed_at IS NULL" id :array-hash)))
	     (multiple-value-bind (asked-latitude asked-longitude) (get-position (flexi-streams:make-in-memory-input-stream (gethash "data" (aref asked-image 0))))
	       (let* ((d (distance input-latitude input-longitude
				   asked-latitude asked-longitude))
		      (correct? (< d 100)))

		 (when correct?
		   (execute
		    "UPDATE pictureguessing_session 
SET guessed_at = NOW()
WHERE user_id = $1 AND picture_id = $2" id (gethash "id" (aref asked-image 0))))
		 
		 (format nil "{\"correct?\": ~a}" (if (< d 100)
						      "true"
						      "false"))))))))

(defroute guess-location ("/api/guess-location" :method :post :decorators (@json @transaction @no-cache @authenticated)) (&get gamesession )
  ;; assuming "latitude", "longitude" & "picture-id"
  (assert gamesession)
  (assert (not (string= gamesession "NIL")))
  (let* ((guess (parse (hunchentoot:raw-post-data :force-text t)))

	 (picture-id (gethash "picture-id" guess))
	 (picture-location (aref (query "SELECT latitude, longitude FROM pictures WHERE id = $1" picture-id :array-hash) 0))
	 (guess-latitude (gethash "latitude" guess))
	 (guess-longitude (gethash "longitude" guess))

	 (pic-latitude (gethash "latitude" picture-location))
	 (pic-longitude (gethash "longitude" picture-location))
	 
	 (dist (distance guess-latitude guess-longitude
			 pic-latitude pic-longitude))
	 (correct? (< dist
		      100.0)))

    (format t "Guessing session id ~a~%" gamesession)

    (execute "INSERT INTO locationguessing_session_guess (session_id, picture_id, correctly_guessed) VALUES ($1, $2, $3)"
	     gamesession
	     picture-id
	     correct?)

    (if correct?
        "{\"correct?\": true}"
	"{\"correct?\": false}")))
	  

(defun load-counties ()
  (when (zerop (caar (query "select count(*) from county")))
    (execute (:insert-rows-into 'county
	      :columns 'code 'name

	      :values (mapcar (lambda (code)
				(let ((result 
					(list (parse-integer (gethash "code" code))
					      (->> (coerce (gethash "classificationItemNames" code) 'list)
						first
						(gethash "name")))))
				  (format t "inserting ~a~%" result)
				  result))
			      (coerce 
			       (parse (slurp-utf-8 #P"/etc/pichunter/counties.json")
				      :allow-comments t)
			       'list))))))

;; curl https://www2.tilastokeskus.fi/fi/luokitukset/corrmaps/export/kunta_1_20160101%23maakunta_1_20160101/ -o maakunnat_kunnat.csv
(defun load-municipalities ()
  (when (zerop (caar (query "SELECT count(*) FROM municipality")))
    (execute (:insert-rows-into 'municipality
	      :columns 'code 'county-code
	      :values
	      (->> (slurp #P"/etc/pichunter/maakunnat_kunnat.csv" :external-format :cp1252)
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
		(remove-if-not #'identity ))))))
  
  

(defun load-codesets ()
  (load-counties)
  (load-municipalities))
  
;; (with-db
;;     (postmodern:with-schema (:pichunter)
;;     (load-codesets)))
