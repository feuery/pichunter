(defpackage pichunter.routes
  (:use :cl )
  (:import-from :pichunter.std :hash-keys)
  (:export :*routes* :defroute))

(in-package :pichunter.routes)



(defparameter *routes* (make-hash-table :test 'equal))

(setf (gethash "GET" *routes*) (make-hash-table :test 'equal))
(setf (gethash "HEAD" *routes*) (make-hash-table :test 'equal))
(setf (gethash "POST" *routes*) (make-hash-table :test 'equal))
(setf (gethash "PUT" *routes*) (make-hash-table :test 'equal))
(setf (gethash "DELETE" *routes*) (make-hash-table :test 'equal))
(setf (gethash "CONNECT" *routes*) (make-hash-table :test 'equal))
(setf (gethash "OPTIONS" *routes*) (make-hash-table :test 'equal))
(setf (gethash "TRACE" *routes*) (make-hash-table :test 'equal))
(setf (gethash "PATCH" *routes*) (make-hash-table :test 'equal))

(defmacro defroute (method url-regexp env-symbol &rest body)
  (let ((method (string-upcase method))
	(url-regexp (if (str:ends-with? "$" url-regexp)
			url-regexp
			(format nil "~a$" url-regexp))))
    (assert (gethash method *routes*) () (format nil "Method ~a not recognised. Should be one of the following: ~a" method (hash-keys *routes*)))
    `(setf (gethash ,url-regexp
		    (gethash ,method *routes*))
	   (lambda (,env-symbol)
	     ,@body))))
