(defpackage pichunter.file-handler
  (:use :cl)
  (:use :pichunter.std)
  (:export :get-picture-data))

(in-package pichunter.file-handler)

(defun get-picture-data (guid)
  (let* ((result 
	   (car
	    (postmodern:query "SELECT data, mime FROM pichunter.pictures WHERE id = $1" guid)))
	 (file-data (car result))
	 (mime (cadr result)))
    (values file-data mime)))
