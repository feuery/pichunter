(defpackage pichunter.file-handler
  (:use :cl)
  (:use :pichunter.std)
  (:export :handle-upload :get-picture-data)
  (:import-from :pichunter.decorators :@transaction))

(in-package pichunter.file-handler)

(defun get-picture-data (guid)
  (pichunter.std:with-db
      (let* ((result 
	      (car
	       (postmodern:query "SELECT data, mime FROM pichunter.pictures WHERE id = $1" guid)))
	     (file-data (car result))
	     (mime (cadr result)))
	(values file-data mime))))
