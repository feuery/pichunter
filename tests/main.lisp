(defpackage pichunter/tests/main
  (:use :cl
	:com.inuoe.jzon
        :postmodern
	:binding-arrows
	:pichunter.std
	:pichunter.migrations
        :fiveam)
  (:export :test-data
	   :logout
	   :login
           :url
	   :test-data :main-suite))
(in-package :pichunter/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :pichunter)' in your Lisp.

(def-suite main-suite)

(defvar *test-server* nil)
(defvar *test-port* 3001)

(defun url ()
  (format nil "http://localhost:~d" *test-port*))

(def-fixture test-data ()
  (with-db
    (format t "Setting up test database ~a~%" *package*)
    (execute "DROP SCHEMA IF EXISTS pichunter_test CASCADE")
    (execute "CREATE SCHEMA IF NOT EXISTS pichunter_test")
    (with-schema (:pichunter_test)
      (execute "CREATE TABLE IF NOT EXISTS migrations_tracker
(
	filename TEXT NOT NULL PRIMARY KEY,
	checksum TEXT NOT NULL,
	installed_successfully BOOLEAN NOT NULL DEFAULT FALSE
)")

      (migrate)
      (pichunter.game-routes:load-codesets)
      (pichunter.user-routes:register "test_user" "Test User" "testpassword")
      (pichunter.user-routes:register "test_nonadmin" "An actual user" "testpassword")

      ;; let's assign the latter user into any kind of a group and allow the basic users to do anything

      (let ((user-id (getf (query "SELECT * FROM users WHERE username = $1 " "test_nonadmin" :plist) :id))
	    (group-id (getf (query "SELECT ID from usergroup where Name = 'Users'" :plist ) :id))
	    (permission-id (getf (query "SELECT ID FROM permission WHERE action = 'view-picture'" :plist) :id)))
	(execute "INSERT INTO grouppermission VALUES ($1, $2)" permission-id group-id)
	(execute "INSERT INTO groupmapping VALUES ($1, $2)" user-id group-id)))

    (when *test-server*
      (hunchentoot:stop *test-server*))
    
    (unwind-protect 
	(progn 
	  (setf *test-server* (pichunter:start-server :port *test-port*))
	  (&body))
    
    (format t "Tearing down test database ~%")
    (execute "DROP SCHEMA IF EXISTS pichunter_test CASCADE"))

    (hunchentoot:stop *test-server*)
    (setf *test-server* nil)))


(defun admin-is-ok (user)

  (is (equalp (gethash "username" user)
	      "test_user"))
  (is (equalp (gethash "displayName" user)
	      "Test User"))

  (is (equalp (gethash "imgId" user)
	      "NULL")
      "admin is is")

  (is (equalp (gethash "abilities" user)
	      (vector "insert-picture" "can-admin" "view-picture")))
  (is (gethash "activated?" user)))

(defun user-is-ok (user)
  (is (equalp (gethash "username" user)
	      "test_nonadmin"))
  (is (equalp (gethash "displayName" user)
	      "An actual user"))
  (is (equalp (gethash "imgId" user)
	      "NULL")
      "user is ok")

  (is (equalp (gethash "abilities" user)
	      (vector "view-picture")))
  (is (not (gethash "activated?" user))))

(defun logout (jar)
  (drakma:http-request (format nil "~a/api/logout" (url))
		       :additional-headers `(("X-pichunter-test" . "true"))
		       :cookie-jar jar)
  (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/session" (url)))
    (is (equalp 401 status))))

(defun login (jar username password)
  (if jar
      (drakma:http-request (format nil "~a/api/login" (url))
			   :method :post
			   :cookie-jar jar
			   :additional-headers `(("X-pichunter-test" . "true"))
			   :content (format nil "{\"username\": \"~a\", \"password\": \"~a\"}" username password))
      (drakma:http-request (format nil "~a/api/login" (url))
			   :method :post
			   :additional-headers `(("X-pichunter-test" . "true"))
			   :content (format nil "{\"username\": \"~a\", \"password\": \"~a\"}" username password))))


(def-test authentication (:fixture test-data :suite main-suite)
  ;; (testing "authentication |"
  ;; (testing " if authentication unauths users with wrong username / password"
  (multiple-value-bind (body status-code) (login nil "rubbish" "none")
    (let ((result (trivial-utf-8:utf-8-bytes-to-string body)))
      (is (equalp 401 status-code))
      (is (equalp "not authorized" result)))

    (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/session" (url)))
      (is (equalp 401 status)))

    ;; (testing " login|"
    (let ((jar (make-instance 'drakma:cookie-jar)))
      (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/login" (url))
							      :method :post
							      :additional-headers `(("X-pichunter-test" . "true"))
							      :content (format nil "{\"username\": \"~a\", \"password\": \"~a\"}"
									       "test_user" "testpassword")
							      :cookie-jar jar)
	(let ((result (trivial-utf-8:utf-8-bytes-to-string body)))
	  (is (equalp 200 status))
	  (is (not (equalp result "")))
	  
	  (let ((user (parse result)))

	    (admin-is-ok user)))
	;; (testing " if session returns now something other than 401"
	(multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/session" (url))
								:additional-headers `(("X-pichunter-test" . "true"))
								:cookie-jar jar)
	  (is (equalp 200 status))

	  (let ((user (parse (trivial-utf-8:utf-8-bytes-to-string body))))
	    (admin-is-ok user))))

      ;; (testing " logout"
      (logout jar)

      ;; (testing " login with a nonadmin"
      (multiple-value-bind (body status) (drakma:http-request (format nil "~a/api/login" (url))
							      :method :post
							      :additional-headers `(("X-pichunter-test" . "true"))
							      :content (format nil "{\"username\": \"~a\", \"password\": \"~a\"}"
									       "test_nonadmin" "testpassword")
							      :cookie-jar jar)
	(let ((result (trivial-utf-8:utf-8-bytes-to-string body)))
	  (is (equalp 200 status))
	  (is (not (equalp result "")))
	  
	  (let ((user (parse result)))

	    (user-is-ok user))))
      ;; (testing " can? decorator"
      (multiple-value-bind (body status)
	  (drakma:http-request (format nil "~a/api/pictures" (url))
			       :additional-headers `(("X-pichunter-test" . "true"))
			       :cookie-jar jar)
	(is (equalp status 401))
	(is (equalp "you need to be able to can-admin"
		    (trivial-utf-8:utf-8-bytes-to-string body))))

      ;; (testing "if authenticated works"
      (logout jar)
      (multiple-value-bind (body status)
	  (drakma:http-request (format nil "~a/api/next-picture/1" (url))
			       :additional-headers `(("X-pichunter-test" . "true"))
			       :cookie-jar jar)
	(is (equalp status 401))
	(is (equalp (trivial-utf-8:utf-8-bytes-to-string body)
		    "not authorized"))))))


;; (run! 'main-suite)
