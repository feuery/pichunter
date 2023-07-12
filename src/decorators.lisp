(defpackage pichunter.decorators
  (:use :cl)
  (:import-from :pichunter.std :with-db)
  (:import-from :postmodern :with-transaction)
  (:export :@json :@transaction))

(in-package pichunter.decorators)

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @transaction (next)
  (with-db 
      (with-transaction ()
	(funcall next))))
