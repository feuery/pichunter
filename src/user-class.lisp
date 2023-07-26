(defpackage :pichunter.user
  (:use :cl :postmodern)
  (:export :user :user-username :user-display-name :user-id :id))

(in-package :pichunter.user)


(defclass user ()
  ((username :initarg username


	     :col-type string
	     :accessor user-username)
   (id :initarg id
       :col-type integer
       :accessor user-id)
   (display_name :initarg display-name
		 :col-type string
		 :accessor user-display-name)
   (img_id :initarg image-id
	   :col-type string
	   :accessor user-image-id))
  (:metaclass dao-class)
  (:keys id))
