(defpackage pichunter/tests/game
  (:use :cl
	:com.inuoe.jzon
	:binding-arrows
   :pichunter.std
   :pichunter.user
	:postmodern
	:fiveam )
  (:import-from :pichunter/tests/main
		:test-data
		:url)
  (:export :game-suite))

(in-package :pichunter/tests/game)

(def-suite game-suite)

(setf fiveam:*run-test-when-defined* t)

(defparameter *jar* nil)

(defun logout ()
  (pichunter/tests/main:logout *jar*))

(defun login* (username password)
  (pichunter/tests/main:login *jar* username password))

(defvar *logged-in-user-id* nil)

(defmacro with-login (username-and-password &rest body)
  (destructuring-bind (username password) username-and-password
    `(let* ((result (parse (trivial-utf-8:utf-8-bytes-to-string
			    (login* ,username ,password))))
	    (*logged-in-user-id* (gethash "id" result)))
       (unwind-protect (progn ,@body)
	 (logout)))))
       

(defun get-next-picture (&key gametype)
  (multiple-value-bind (body status1) (drakma:http-request (if gametype
							       (format nil "~a/api/next-picture/6?gametype=~a" (url) gametype)
							       (format nil "~a/api/next-picture/6" (url)))
							  :cookie-jar *jar*
							  :additional-headers `(("X-pichunter-test" . "true")))
    (is (not (zerop (caar (query "SELECT count(*) from pictures")))))
    (is (equalp 200 status1))
    (parse body)))

(defun describe-hash (hash)
  (dolist (key (hash-keys hash))
    (format t "~a -> ~a~%" key (gethash key hash))))

(defun guess-pic-picture (session-id picture &key guess-wrong)
  (assert session-id)
  (let ((url (format nil "~a/api/picture/guess?gametype=picguess&gamesession=~a" (url) session-id))
	(lat (gethash "latitude" picture))
	(lng (gethash "longitude" picture)))
    (format t "url: ~a~%" url)
    
    (is-true lat)
    (is-true lng)
    (multiple-value-bind (body status2) (drakma:http-request url
							     :cookie-jar *jar*
							     :additional-headers `(("X-pichunter-test" . "true")
										   ("X-PICHUNTER-TEST-LAT" . ,(if guess-wrong
														  (+ 20 lat)
														  lat))
										   ("X-PICHUNTER-TEST-LNG" . ,(if guess-wrong
														  (+ 20 lng)
														  lng)))
							     :method :post
							     
							     :content nil)

      (is (equalp 200 status2))
      (if guess-wrong
	  (is (not (gethash "correct?"
			    (parse body))))
	  (is (gethash "correct?"
		   (parse body)))))))

(defun guess-pic (session-id picture)
  (assert session-id)
  (format t "Guessing picture ~a~%" picture)
  (let ((url (format nil "~a/api/location/guess?gamesession=~a" (url) session-id)))
    (format t "url: ~a~%" url)
    (multiple-value-bind (body status2) (drakma:http-request url
							     :cookie-jar *jar*
							     :additional-headers `(("X-pichunter-test" . "true"))
							     :method :post
							     
							     :content (format nil "{\"latitude\": ~f, \"longitude\": ~f, \"picture-id\": \"~a\"}"
									      (gethash "latitude" picture)
									      (gethash "longitude" picture)
									      (gethash "id" picture)))

      (is (equalp 200 status2))
      (is (gethash "correct?"
		   (parse body))))))

(defun fill-picture-data ()
  (dotimes (index 6)
    (dotimes (county-code 10)
      (let ((county-code (1+ county-code)))
	(unless (equalp county-code 3)
	  (execute "insert into pictures (filename, mime, latitude, longitude, data, county_code) values ($1, $2, $3, $4, $5, $6)"
		   (format nil "game ~d ~d.lisp" county-code index)
		   "application/jpeg"
		   0
		   0 
		   (slurp-bytes #P"/dev/null")
		   county-code))))))

(def-test picture-game (:fixture test-data :suite game-suite)
  (with-db
    (with-schema (:pichunter_test)
      (fill-picture-data)

      (let ((*jar* (make-instance 'drakma:cookie-jar)))
	(multiple-value-bind (body status) (login* "test_nonadmin" "testpassword")
	  (is (equalp 200 status)))
	(let* ((pictures '())
	       (picture (get-next-picture :gametype "picguess"))
	       (session-id (gethash "session-id" picture)))
	    (is-true session-id)
	    (format t "Session-id: ~a ~%" session-id)
	    (is (equalp (hash-keys picture)
			(list "id" "filename" "latitude" "longitude" "approver" "session-id")))
	    
	    (push picture pictures)

	  ;; there's only one, unfinished guess
	  (is-true (not (query "SELECT * FROM game_session_guess WHERE guessed_at IS NOT NULL")))

	  ;; amount of sessions is 1 and they're all of the gametype "picture"
	  
	  (is (equalp (list "picture")
		      (map 'list
			   (partial #'gethash "gametype")
			   (query "SELECT session.gametype FROM game_session_guess guess JOIN game_session session ON guess.session_id = session.id" :array-hash))))

	  (is (equalp (list "picture")
		      (map 'list
			   (partial #'gethash "gametype")
			   (query "SELECT gametype FROM game_session session" :array-hash))))
	  ;; guess the first picture away
	  (guess-pic-picture session-id picture)

	  ;; the amount of correctly guessed guesses == the amount of calls to guess-pic-picture
	  (is (equalp 1
		      (length (query "SELECT * FROM game_session_guess where correctly_guessed"))))

	  (is (equalp 1
		      (length (query "SELECT * FROM game_session_guess WHERE guessed_at IS NOT NULL"))))

	  ;; getting next picture initializes the guess table with a new row on the same session
	  (let ((pic (get-next-picture :gametype "picguess")))
	    (push pic pictures)
	    (is (equalp 1
			(length (query "SELECT * FROM game_session_guess WHERE guessed_at IS NULL"))))
	    (is (string= (gethash "session-id" pic)
			 session-id))
 
	    (guess-pic-picture session-id picture :guess-wrong t)

	    (dotimes (x 7)
	      (let ((picture (get-next-picture :gametype "picguess")))
		(unless (equalp 'NULL picture)
		  (is (hash-table-p picture))
		  (push picture pictures)
		  (guess-pic-picture session-id picture)

		  (is (equalp (+ 2 x)
			      (length (query "SELECT * FROM game_session_guess where correctly_guessed"))))
		  (is (equalp (+ 2 x)
			      (length (query "SELECT distinct picture_id FROM game_session_guess where correctly_guessed"))))))))

	  (multiple-value-bind (session-body status)
		(drakma:http-request (format nil "~a/api/picture/session" (url))
				     :cookie-jar *jar*
				     :additional-headers `(("X-pichunter-test" . "true"))
				     :method :get)
	      (let* ((session (trivial-utf-8:utf-8-bytes-to-string session-body))
		     (session-obj (parse session)))
		(is (equalp 200 status))
		(is-true session)
		(is (equalp 9
			    (length (gethash "guesses" session-obj))))
	        
		(is (equalp (map 'list
				 (partial #'gethash "correctly_guessed")
				 (gethash "guesses" session-obj))
			    (list T nil T T T T T T T))))))))))

    
(def-test location-game (:fixture test-data :suite game-suite)
  (with-db  
      (with-schema (:pichunter_test)

	(fill-picture-data)
	
	(let ((*jar* (make-instance 'drakma:cookie-jar)))
	  (multiple-value-bind (body status) (login* "test_nonadmin" "testpassword")
	    (is (equalp 200 status)))
	  
	  (let* ((pictures '())
		 (picture (get-next-picture))
		 (session-id (gethash "session-id" picture)))
	    (is-true session-id)
	    (format t "Session-id: ~a ~%" session-id)
	    (is (equalp (hash-keys picture)
			(list "id" "filename" "latitude" "longitude" "approver" "session-id")))
	    
	    (push picture pictures)

	    (is (not (query "SELECT * FROM game_session_guess")))
	    
	    (guess-pic session-id picture)

	    (is (equalp 1
			(length (query "SELECT * FROM game_session_guess where correctly_guessed"))))

	    (dotimes (x 7)
	      (let ((picture (get-next-picture)))
		(unless (equalp 'NULL picture)
		  (format t "picture: ~a ~a ~%~%" picture (type-of picture))
		  (is (hash-table-p picture))
		  (push picture pictures)
		  (guess-pic session-id picture)

		  (is (equalp (+ 2 x)
			      (length (query "SELECT * FROM game_session_guess where correctly_guessed"))))
		  (is (equalp (+ 2 x)
			      (length (query "SELECT distinct picture_id FROM game_session_guess where correctly_guessed")))))))	    

	    (is (equalp 8
			(length (remove-duplicates (mapcar (partial #'gethash "id") pictures)
						   :test #'string=))))

	    (is (equalp 8
			(length (query "SELECT * FROM game_session_guess WHERE correctly_guessed"))))

	    (multiple-value-bind (session-body status)
		(drakma:http-request (format nil "~a/api/location/session" (url))
				     :cookie-jar *jar*
				     :additional-headers `(("X-pichunter-test" . "true"))
				     :method :get)
	      (let* ((session (trivial-utf-8:utf-8-bytes-to-string session-body))
		     (session-obj (parse session)))
		(is (equalp 200 status))
		(is-true session)
		(is (equalp 8
			    (length (gethash "guesses" session-obj))))
	        
		(is (equalp (map 'list
				 (partial #'gethash "correctly_guessed")
				 (gethash "guesses" session-obj))
			    (list  t  t  t  t  t  t  t  t ))))))

	  (execute "UPDATE game_session SET completed_at = now()")

	  ;; start a new session
	  (let* ((pictures '())
		 (picture (get-next-picture))
		 (session-id (gethash "session-id" picture)))
	    (push picture pictures)

	    (is (query "SELECT * FROM game_session_guess"))
	    
	    (guess-pic session-id picture)

	    (dotimes (x 3)
	      (let ((picture (get-next-picture)))
		(unless (equalp 'NULL picture)
		  (push picture pictures)
		  (guess-pic session-id picture)))))

	  (execute "UPDATE game_session SET completed_at = now()")

	  (logout)

	  (with-login ("test_nonadmin" "testpassword")
	    (let* ((user (query "SELECT id, username, display_name, img_id FROM users WHERE id = $1" *logged-in-user-id*
				(:dao user :single)))
		   (result (pichunter.user-routes:get-highest-scores-per-session user)))
	      (is (equalp 8 (gethash "correct_guesses"
				     (gethash "location" result))))

	      (is (equalp 8 (gethash "all_guesses"
				      (gethash "location" result))))
	      (is (null (gethash "picture" result)))))))))
