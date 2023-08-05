(defpackage pichunter.file-handler
  (:use :cl :pichunter.std :postmodern :pichunter.config :binding-arrows :com.inuoe.jzon)
  (:import-from :easy-routes :defroute)
  (:import-from :pichunter.decorators :@can? :@json :@transaction :@authenticated)
  (:export :get-picture-data))

(in-package pichunter.file-handler)

(defun get-picture-data (guid)
  (let* ((result 
	   (car
	    (postmodern:query "SELECT data, mime FROM pictures WHERE id = $1" guid)))
	 (file-data (car result))
	 (mime (cadr result)))
    (values file-data mime)))

(defun get-picture (guid)
  (when guid
      (multiple-value-bind (picture-data mime) (get-picture-data guid)
	(setf (hunchentoot:content-type*) mime)
	picture-data)))

(defroute get-picture-route ("/api/pictures/:guid" :method :get :decorators (@transaction)) ()
  (get-picture guid))

;; https://gis.stackexchange.com/a/273402
(defun coordinate->number (coord)
  (float (+ (first coord)
	    (/ (second coord) 60)
	    (/ (third coord) 3600))))


(defun query-municipality-from-mml (lon lat)
  (let ((url (format nil "https://avoin-paikkatieto.maanmittauslaitos.fi/geocoding/v2/pelias/reverse?&lang=fi&sources=addresses&crs=EPSG:3067&point.lon=~f&point.lat=~f" lon lat)))
    (->> (coerce (->> (drakma:http-request url
					   :basic-authorization (list (config :mml-api-key) ""))
		   (trivial-utf-8:utf-8-bytes-to-string)
		   parse 
		   (gethash "features"))
		 'list)
      (first)
      (gethash "properties")
      (gethash "kuntatunnus"))))

(defroute picture-upload-route ("/api/pictures" :method :post :decorators (@json @transaction @authenticated (@can? "insert-picture"))) (&post file)
  (destructuring-bind (tmp-file filename mime) file
    (let ((bytes (slurp-bytes tmp-file))
	  (exif (handler-case
		    (zpb-exif:make-exif tmp-file)
		  (zpb-exif:invalid-jpeg-stream (e) nil)
		  (zpb-exif:invalid-exif-stream (e) nil))))
      (format t "inside picture-upload-route, ~a ~a ~a ~%" filename mime (prin1-to-string exif))
      (when exif
	(let* ((gps-data (zpb-exif:ifd-alist (zpb-exif:gps-ifd exif)))
	       (latitude (coerce (cdr (assoc "GPSLatitude" gps-data :test #'string=)) 'list))
	       (latitude-number (coordinate->number latitude))
	       (longitude (coerce (cdr (assoc "GPSLongitude" gps-data :test #'string=)) 'list))
	       (longitude-number (coordinate->number longitude))
	       (_ (format t "into query-municipality-from-mml~%"))
	       (municipality-code (query-municipality-from-mml longitude-number latitude-number))
	       (_ (format t "out of query-municipality-from-mml~%"))
	       (county-code (first (query (:select 'county_code
					   :from 'municipality
					   :where (:= 'code municipality-code)) :list))))
	  

	  (format t "latitude: ~a~%longitude: ~a~%mml-data~a~%"
		latitude-number
		longitude-number
		municipality-code)

	  (execute "insert into pictures (filename, mime, latitude, longitude, data, county_code) values ($1, $2, $3, $4, $5, $6)"
		   filename
		   mime
		   latitude-number
		   longitude-number
		   bytes
		   county-code)
	  "{\"success\": true}")))))
