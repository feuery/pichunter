(defpackage :pichunter.e2e
  (:use :cl :postmodern :pichunter.std)
  (:import-from :pichunter.user-routes :find-by-username :insert-user)
  (:export :setup-e2e))

(in-package :pichunter.e2e)

(defun setup-e2e ()
  (unless (find-by-username "playwright")
    (insert-user "playwright" (sha-512 "p4ssw0rd") "Test User")
    (format t "Inserted playwright user~%"))
  (let ((user-id (getf (first (find-by-username "playwright")) :id))
	(group-id (getf (first (query "SELECT id FROM usergroup WHERE name = 'Users'" :plists)) :id)))
    (execute "UPDATE users SET activated = true, banned = false, display_name = 'Test User' WHERE id = $1" user-id)
    (execute "INSERT INTO groupmapping VALUES ($1, $2) ON CONFLICT DO NOTHING" user-id group-id)
    (execute "INSERT INTO grouppermission SELECT permission.id, usergroup.id FROM permission JOIN usergroup ON usergroup.name = 'Users' ON CONFLICT DO NOTHING")
    (format t "updated user mapping as correct~%")
    (format t "users: ~a~%" (query "SELECT * FROM users" :plists ))))


	;; TODO commit this transaction, after which:
	;; TODO call the playwright script to run the actual e2e-tests and scrape their results
	;; TODO report them
	;; TODO and Idunno rollback this transaction?
	
