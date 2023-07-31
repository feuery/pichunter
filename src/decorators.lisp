(defpackage pichunter.decorators
  (:use :cl :postmodern :pichunter.user)
  (:import-from :pichunter.std :with-db)
  (:import-from :postmodern :with-transaction)
  (:export :@json :@transaction :@no-cache :@authenticated :*user*))

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
	  (funcall next))))))

(defun @no-cache (next)
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (funcall next))

(defparameter *user* nil "A special variable for storing the logged in user (as defined in the db)")
(defun @authenticated (next)
  (let ((user-id (hunchentoot:session-value :logged-in-user-id)))
    (if user-id
	(let ((user (query "SELECT id, username, display_name, img_id FROM users WHERE id = $1" user-id (:dao user :single))))
	  (if (and user
		   (string= (hunchentoot:session-value :logged-in-username)
			    (user-username user)))
	      (let ((*user* user))
		(funcall next))
	      (progn
		(setf (hunchentoot:return-code*) 401)
		"not authorized")))
	(progn
	  (setf (hunchentoot:return-code*) 401)
	  "not authorized"))))

;; (defun @can? (ability next)
  
