(defpackage pichunter.file-handler
  (:use :cl)
  (:use :pichunter.std)
  (:export :handle-upload :get-picture-data)
  (:import-from :pichunter.decorators :@transaction))

(in-package pichunter.file-handler)

(defparameter file-stuff nil)
(defparameter *size* nil)

(defun slurp-stream (stream)
 (concatenate 'vector
                (loop for byte = (read-byte stream nil)
                   while byte
                   collect byte)))
  
  ;; (let* (;;(result nil)
  ;; 	 (size (file-length stream))
  ;; 	 (result (make-array size :element-type :byte )))
    
  ;; 	;; (line (read-byte stream nil nil)))

    
  ;;   ;; (loop while line
  ;;   ;; 	  do (progn
  ;;   ;; 	       (incf size)
  ;;   ;; 	       (setf line (read-byte stream nil nil))))

  ;;   (read-sequence result stream)
  ;;   (setf file-stuff result)
  ;;   (setf *size* size)
  ;;   result)
  

	       
	     
  
(defun handle-upload (files)
  ;; (format t "~a~%" env)
  ;; (setf file-stuff env)
  (pichunter.std:with-db 
      (dolist (file files)
	(let ((stream (second file))
	      (name (third file)))

	  (postmodern:execute "INSERT INTO pichunter.pictures (filename, data) VALUES ($1, $2)"
			      name
			      (slurp-stream stream))))
    ""))

(defun get-picture-data (guid)
    (assert guid)
    (pichunter.std:with-db
	(let ((result 
		(caar
		 (postmodern:query "SELECT data FROM pichunter.pictures WHERE id = $1" guid))))
	  result)))
