(defpackage pichunter/tests/game
  (:use :cl
	:com.inuoe.jzon
	:pichunter.std
	:postmodern
	:rove )
  (:import-from :pichunter/tests/main
		:do-setup
		:url
		:do-teardown))

(in-package :pichunter/tests/game)

(do-setup)
(do-teardown)

(defparameter *jar* nil)

(defun logout ()
  (pichunter/tests/main:logout *jar*))

(defun login* (username password)
  (pichunter/tests/main:login *jar* username password))

(defun get-next-picture ()
  (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/next-picture/6" (url))
							  :cookie-jar *jar*
							  :additional-headers `(("X-pichunter-test" . "true")))
    (ok (not (zerop (caar (query "SELECT count(*) from pictures")))))
    (ok (equalp 200 status))
    (parse body)))

(defun describe-hash (hash)
  (dolist (key (hash-keys hash))
    (format t "~a -> ~a~%" key (gethash key hash))))


(defun guess-pic (session-id picture)
  (assert session-id)
  (format t "Guessing picture ~a~%" picture)
  (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/guess-location?gamesession=~a" (url) session-id)
							  :cookie-jar *jar*
							  :additional-headers `(("X-pichunter-test" . "true"))
							  :method :post
							  
							  :content (format nil "{\"latitude\": ~f, \"longitude\": ~f, \"picture-id\": \"~a\"}"
									   (gethash "latitude" picture)
									   (gethash "longitude" picture)
									   (gethash "id" picture)))

    (ok (equalp 200 status))
    (ok (gethash "correct?"
		 (parse body)))))

(deftest game
  (with-db  
      (with-schema (:pichunter_test)

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
			 county-code)))))
	
	
	(let ((*jar* (make-instance 'drakma:cookie-jar)))
	  (multiple-value-bind (body status) (login* "test_nonadmin" "testpassword")
	    (ok (equalp 200 status)))
	  
	  (let* ((pictures '())
		 (picture (get-next-picture))
		 (session-id (gethash "session-id" picture)))
	    (ok session-id)
	    (format t "Session-id: ~a ~%" session-id)
	    (ok (equalp (hash-keys picture)
			(list "id" "filename" "latitude" "longitude" "session-id")))
	    
	    (push picture pictures)

	    (ok (not (query "SELECT * FROM locationguessing_session_guess")))
	    
	    (guess-pic session-id picture)

	    (ok (equalp 1
			(length (query "SELECT * FROM locationguessing_session_guess where correctly_guessed"))))

	    (dotimes (x 7)
	      (let ((picture (get-next-picture)))
		(unless (equalp 'NULL picture)
		  (format t "picture: ~a ~a ~%~%" picture (type-of picture))
		  (ok (hash-table-p picture))
		  (push picture pictures)
		  (guess-pic session-id picture)

		  (ok (equalp (+ 2 x)
			      (length (query "SELECT * FROM locationguessing_session_guess where correctly_guessed"))))
		  (ok (equalp (+ 2 x)
			      (length (query "SELECT distinct picture_id FROM locationguessing_session_guess where correctly_guessed")))))))

	    

	    (ok (equalp 8
			(length (remove-duplicates (mapcar (partial #'gethash "id") pictures)
						   :test #'string=))))

	    (ok (equalp 8
			(length (query "SELECT * FROM locationguessing_session_guess WHERE correctly_guessed")))))))))
			       
