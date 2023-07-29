(defpackage pichunter/tests/main
  (:use :cl
        :postmodern
	:binding-arrows
	:pichunter.std
	:pichunter.migrations
        :rove))
(in-package :pichunter/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :pichunter)' in your Lisp.

;; (defun init-db ()

(defvar *test-server* nil)
(defvar *test-port* 3001)

(defun url ()
  (format nil "http://localhost:~d" *test-port*))

(setup
 (with-db 
   (format t "Setting up test database ~%")
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
     (pichunter.user-routes:register "test_user" "Test User" "testpassword"))
   (setf *test-server* (pichunter:main :port *test-port*))))

(teardown
  (with-db 
    (format t "Tearing down test database ~%")
    (execute "DROP SCHEMA IF EXISTS pichunter_test CASCADE"))

  (hunchentoot:stop *test-server*)
  (setf *test-server* nil))

(deftest authentication
  (testing " if authentication unauths users with wrong username / password"
    (multiple-value-bind (body status-code) (drakma:http-request (format nil "~a/api/login" (url))
								 :method :post
								 :additional-headers `(("X-pichunter-test" . "true"))
								 :content "{\"username\": \"rubbish\", \"password\": \"none\"}")
      (let ((result (trivial-utf-8:utf-8-bytes-to-string body)))
	(ok (equalp 401 status-code))
	(ok (equalp "not authorized" result))))))
