(defpackage pichunter.decorators
  (:use :cl)
  (:import-from :pichunter.std :with-db)
  (:import-from :postmodern :with-transaction)
  (:export :@json :@transaction :@no-cache))

(in-package pichunter.decorators)

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @transaction (next)
  (with-db 
      (with-transaction ()
	(funcall next))))

(defun @no-cache (next)
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (funcall next))
