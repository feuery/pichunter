(defpackage :pichunter.e2e
  (:use :cl :postmodern :pichunter.std)
  (:import-from :pichunter.user-routes :find-by-username :insert-user)
  (:export :setup-e2e))

(in-package :pichunter.e2e)

(defmacro be-advisable (fn-list &rest body)
  (assert (equalp (length fn-list) 2))
  (let ((fn-symbol (first fn-list))
	(advice-symbol (second fn-list)))
    `(unwind-protect
	  (progn
	    (cl-advice:make-advisable ,fn-symbol)
	    (cl-advice:add-advice :around ,fn-symbol ,advice-symbol)
	    ,@body)
       (cl-advice:make-unadvisable ,fn-symbol))))

(defun mocked-query-municipality-from-mml (next-fn lon lat)
  "837")

(defun setup-e2e ()
  (unless (find-by-username "playwright")
    (insert-user "playwright" (sha-512 "p4ssw0rd") "Test User")
    (format t "Inserted playwright user~%"))
  (let ((user-id (getf (first (find-by-username "playwright")) :id))
	(group-id (getf (first (query "SELECT id FROM usergroup WHERE name = 'Users'" :plists)) :id)))
    (execute "UPDATE users SET activated = true, banned = false, display_name = 'Test User' WHERE id = $1" user-id)
    (execute "INSERT INTO groupmapping VALUES ($1, $2) ON CONFLICT DO NOTHING" user-id group-id)
    (execute "INSERT INTO grouppermission SELECT permission.id, usergroup.id FROM permission JOIN usergroup ON usergroup.name = 'Users' ON CONFLICT DO NOTHING")

    (be-advisable
     ('pichunter.file-handler:query-municipality-from-mml 'mocked-query-municipality-from-mml)
     (pichunter.file-handler:import-picture "testimage.jpg" "image/jpeg" (in-compile-path "testimage.jpg")))

    (execute "UPDATE pictures SET approver = $1" user-id)
    (format t "updated user mapping as correct~%")))
       
;; (defun se-parempi-fn (next &rest args)
;;   (error "Oho kutsuit oikeaa make-exifiä"))

;; (unwind-protect
;;      (progn
;;        (cl-advice:make-advisable 'zpb-exif:make-exif)
;;        (cl-advice:add-advice :around 'zpb-exif:make-exif 'se-parempi-fn)
;;        (pichunter.file-handler:import-picture "testimage.jpg" "image/jpeg" (in-compile-path "testimage.jpg")))
;;   (cl-advice:make-unadvisable 'zpb-exif:make-exif))
