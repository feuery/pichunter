(defpackage pichunter.decorators
  (:use :cl :postmodern :pichunter.user :com.inuoe.jzon )
  (:import-from :pichunter.std :with-db)
  (:import-from :postmodern :with-transaction)
  (:export :@json :@can? :@transaction :@no-cache :@authenticated :*user*))

(in-package pichunter.decorators)

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @transaction (next)
  (let* ((test? (equalp "true"
			(hunchentoot:header-in* "X-PICHUNTER-TEST")))
	 (schema (if test?
		     :pichunter_test
		     :pichunter)))
    (with-db 
      (with-schema (schema :if-not-exist nil)
	(with-transaction ()
	  (format t "inside schema ~a~%" schema)
	  (format t "search-path: ~a~%" (postmodern:get-search-path))
	  (funcall next))))))

(defun @no-cache (next)
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (funcall next))

(defparameter *user* nil "A special variable for storing the logged in user (as defined in the db)")
(defun @authenticated (next)
  (let ((user-id (hunchentoot:session-value :logged-in-user-id)))
    (if user-id
	(let ((user (query "SELECT users.id, users.username, users.display_name, users.img_id, json_agg(distinct ab.action) AS \"permissions\" FROM users JOIN  user_abilities ab ON users.id = ab.id WHERE users.id = $1 GROUP BY users.id" user-id (:dao user :single))))
	  (if (and user
		   (string= (hunchentoot:session-value :logged-in-username)
			    (user-username user)))
	      (let ((*user* user))
		(setf (user-permissions *user*)
		    (coerce (parse (user-permissions *user*))
			    'list))
		(funcall next))
	      (progn
		(setf (hunchentoot:return-code*) 401)
		"not authorized")))
	(progn
	  (setf (hunchentoot:return-code*) 401)
	  "not authorized"))))

(defun @can? (ability next)
  (if (and *user*
	   (member ability 
		   (coerce (user-permissions *user*) 'list)
		   :test #'string=))
      (funcall next)
      (progn
	(setf (hunchentoot:return-code*) 401)
	(format nil "you need to be able to ~a" ability))))
