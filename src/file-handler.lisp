(defpackage pichunter.file-handler
  (:use :cl :pichunter.std :postmodern)
  (:import-from :easy-routes :defroute)
  (:import-from :pichunter.decorators :@json :@transaction)
  (:export :get-picture-data))

(in-package pichunter.file-handler)

(defun get-picture-data (guid)
  (let* ((result 
	   (car
	    (postmodern:query "SELECT data, mime FROM pichunter.pictures WHERE id = $1" guid)))
	 (file-data (car result))
	 (mime (cadr result)))
    (values file-data mime)))

(defun get-picture (guid)
  (when guid
      (multiple-value-bind (picture-data mime) (get-picture-data guid)
	(setf (hunchentoot:content-type*) mime)
	picture-data)))

(defroute get-picture-route ("/api/pictures/:guid" :method :get :decorators (@transaction)) ()
  (format t "get-picture-route ~a~%" guid)
  (get-picture guid))

;; https://gis.stackexchange.com/a/273402
(defun coordinate->number (coord)
  (float (+ (first coord)
	    (/ (second coord) 60)
	    (/ (third coord) 3600))))

(defroute picture-upload-route ("/api/pictures" :method :post :decorators (@json @transaction)) (&post file)
  (destructuring-bind (tmp-file filename mime) file
    (let ((bytes (slurp-bytes tmp-file)))
      
      (let* ((exif (zpb-exif:make-exif tmp-file))
	     (gps-data (zpb-exif:ifd-alist (zpb-exif:gps-ifd exif)))
	     (latitude (coerce (cdr (assoc "GPSLatitude" gps-data :test #'string=)) 'list))
	     (latitude-number (coordinate->number latitude))
	     (longitude (coerce (cdr (assoc "GPSLongitude" gps-data :test #'string=)) 'list))
	     (longitude-number (coordinate->number longitude)))

	(format t "latitude: ~a~%longitude: ~a~%"
		latitude-number
		longitude-number)

	(execute "insert into pichunter.pictures (filename, mime, latitude, longitude, data) values ($1, $2, $3, $4, $5)"
	       filename
	       mime
	       latitude-number
	       longitude-number
	       bytes))
      "{\"success\": false}")))
