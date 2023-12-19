(defpackage :pichunter.game-routes
  (:use :cl :postmodern :pichunter.std :com.inuoe.jzon :binding-arrows :com.inuoe.jzon :pichunter.decorators :pichunter.user)
  (:import-from :pichunter.file-handler :coordinate->number)
  (:import-from :easy-routes :defroute)
  (:import-from :halisql :defqueries)
  (:export :get-position :load-codesets :*test-position-lat* :*test-position-lng*))

(in-package :pichunter.game-routes)

;; (ql:quickload :log4cl.log4slime)
;; (log4cl.log4slime:install)


(defqueries "game-routes.sql")

(defun init-session (game-type user-id gamesession)
  (if gamesession
      gamesession
      (let ((session-id (getf (get-session-id user-id game-type) :id)))
	(if session-id
	    session-id
	    (caar (insert-new-session user-id game-type))))))
  

(defroute next-image ("/api/next-picture/:county-code" :method :get :decorators (@json @transaction @no-cache @authenticated)) (&get gametype gamesession)
  (let* ((db-gametype (if (string= gametype "picguess")
			  "picture"
			  "location"))
		       
	 (gamesession (init-session db-gametype (user-id *user*) gamesession))
	 (id (user-id *user*))
	 (results (coerce (get-next-pic id db-gametype) 'list )))

    (log:info "found ~d results for the next image in game type ~a ~% game session id is ~a~% the results: ~a~%" (length results) gametype gamesession results)

    (when (and (pichunter.std:e2e?)
	       (not (coerce (query "SELECT id FROM pictures") 'list)))
      (log:warn "pictures table seems to be empty"))
    
    (if results
	(let ((result (first results)))
	  (if (and (string= gametype "picguess")
		   (not (query
			 "SELECT *
FROM game_session_guess guess
JOIN game_session session ON session.id = guess.session_id
WHERE guessed_at IS NULL AND session.user_id = $1 AND session.gametype = $2" id db-gametype)))
	    (execute "INSERT INTO game_session_guess (session_id, picture_id, correctly_guessed) VALUES ($1, $2, $3)"
		     gamesession
		     (gethash "id" result)
		     nil)
	    (format t "DID NOT INSERT A GUESS ~%"))

	  (setf (gethash "session-id" result) gamesession)
	  
	  (stringify result))
	"null")))

(defun get-position (tmp-file)
  (if (not pichunter.std:*automatic-tests-mode*)
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
		  (values latitude-number longitude-number)))))))
      (progn
	(format t "get-position in X-PICHUNTER-TEST mode, returning ~a, ~a~%" *test-position-lat* *test-position-lng*)
	(values *test-position-lat* *test-position-lng*))))

(defun distance (lat1 long1
		 lat2 long2)
  (let* ((loc1 (cl-geocode:make-location :latitude lat1 :longitude long1))
	 (loc2 (cl-geocode:make-location :latitude lat2 :longitude long2))
	 (distance (cl-geocode:distance-between loc1 loc2 :unit :kilometers)))
    (* distance 1000)))

(defroute guess-picture ("/api/picture/guess" :method :post :decorators (@json @transaction @no-cache @authenticated @setup-test-headers)) (&post file &get gamesession)
  (assert gamesession)
  (destructuring-bind (tmp-file filename mime) (or file (list nil nil nil))
    (multiple-value-bind (input-latitude input-longitude) (get-position tmp-file)
      (let ((asked-image (query
			  "SELECT pic.id, pic.latitude, pic.longitude, guess.id AS \"guess_id\"
FROM game_session session
JOIN game_session_guess guess ON session.id = guess.session_id
JOIN pictures pic ON guess.picture_id = pic.id
WHERE session.id = $1 AND session.user_id = $2 AND session.gametype = $3 AND guess.guessed_at IS NULL"
			  gamesession
			  (user-id *user*)
			  "picture" :array-hash)))
	(assert (equalp 1
			(length (coerce asked-image 'list))))
	

	(let* ((asked-image (aref asked-image 0))
	       (asked-latitude (gethash "latitude" asked-image))
	       (asked-longitude (gethash "longitude" asked-image))
	       (d (distance input-latitude input-longitude
			    asked-latitude asked-longitude))
	       (correct? (< d 100)))

	  (format t "distance between ~a and ~a is ~a~%"
		  (list input-latitude input-longitude)
		  (list asked-latitude asked-longitude)
		  d)
	  (execute
	   "UPDATE game_session_guess
SET correctly_guessed = $1::boolean,
    guessed_at = NOW()
WHERE ID = $2" correct? (gethash "guess_id" asked-image))
	  
	  (format nil "{\"correct?\": ~a}" (if (< d 100)
					       "true"
					       "false")))))))

(defun fix-timestamp (timestamp)
  (multiple-value-bind (year month day hour minute second millisec)
      (simple-date:decode-timestamp timestamp)
    (let ((obj (make-hash-table :test 'equal :size 7)))
      (setf (gethash "year" obj) year)
      (setf (gethash "month" obj) month)
      (setf (gethash "day" obj) day)
      (setf (gethash "hour" obj) hour)
      (setf (gethash "minute" obj) minute)
      (setf (gethash "second" obj) second)
      (setf (gethash "millisec" obj) millisec)
      obj)))

(defroute session-location ("/api/:gametype/session" :method :get :decorators (@json @transaction @no-cache @authenticated)) ()
  (assert (or (string= gametype
		       "location")
	      (string= gametype
		       "picture")))
  (let ((session-data (query "SELECT ID, started_at, completed_at
FROM game_session
WHERE completed_at IS NULL
  AND user_id = $1
  and gametype = $2" (user-id *user*) gametype :array-hash)))
    (if (coerce session-data 'list)	
	(let* ((session-root (aref session-data 0))
	       (session-id (gethash "id" session-root ))
	       (guesses (query
			 "SELECT picture_id, correctly_guessed
FROM game_session_guess
WHERE session_id = $1" session-id :array-hash)))
	  (setf (gethash "started_at" session-root)
		(fix-timestamp (gethash "started_at" session-root)))
	  (if (equalp (gethash "completed_at" session-root)
		      :null)
	      (setf (gethash "completed_at" session-root) nil)
	    (setf (gethash "completed_at" session-root)
		  (fix-timestamp (gethash "completed_at" session-root))))
	  
	  (setf (gethash "guesses" session-root) guesses)

	  (stringify session-root))
	(progn
	  (format t "~&Session of ~a is empty...~%" gametype)
	  (setf (hunchentoot:return-code*) 404)
	  ""))))

(defroute guess-location ("/api/location/guess" :method :post :decorators (@json @transaction @no-cache @authenticated)) (&get gamesession )
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

    (execute "INSERT INTO game_session_guess (session_id, picture_id, correctly_guessed) VALUES ($1, $2, $3)"
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
