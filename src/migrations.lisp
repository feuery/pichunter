(defpackage pichunter.migrations
  (:use :cl :postmodern)
  (:import-from :pichunter.std :if-let :with-db)
  (:export :migrate :clean))

(in-package pichunter.migrations)

(defmacro slurp-queries (file-path)
  "Reads an sql-file into a compile-time constant you can push to exec-all"
  `(list ,@(read-queries file-path)))

(defun exec-all (queries)
  (dolist (query queries)
    (execute query)))


(defun init-migration-system ()
  (with-db
      (exec-all
       (slurp-queries #P"init-migration-tables.sql"))))

(defparameter migrations nil)

(defclass migration ()
  ((name :initarg :filename
	 :accessor migration-filename
	 :initform (error "What file is the migration from?"))
   (sql-code :initarg :code
	     :initform (error "Please provide the code")
	     :accessor migration-code)
   (checksum :accessor migration-checksum)))

(defmethod initialize-instance :after ((migration migration) &key)
  (with-slots (sql-code checksum) migration
    (setf checksum (sxhash sql-code))))


(defmacro defmigration (path-to-sql)
  (let ((filename (pathname-name path-to-sql)))
    `(let ((found-migration? nil))
       (dolist (migration migrations)
	 (when (string= (migration-filename migration) ,filename)
	   (setf (migration-code migration)
		 (slurp-queries ,path-to-sql))
	   (setf found-migration? t)))
       (unless found-migration?
	 (setf migrations (cons
			   (make-instance 'migration
					  :filename ,filename
					  :code (slurp-queries ,path-to-sql))
			   migrations))))))

(defun clean ()
  (execute "DROP SCHEMA pichunter CASCADE")
  (init-migration-system))

(defun migrate ()
  (let* ((migrations (reverse migrations))
	 (old-migrations (query "SELECT * FROM pichunter.migrations_tracker" :alists))
	 (migrations-to-run (remove-if-not (lambda (new-migration)
					     (if-let (old (first (remove-if-not (lambda (old-migration)
										  (string= (migration-filename new-migration)
											   (cdr (assoc :filename old-migration))))
										old-migrations)))
					       
					       (progn
						 (unless (equalp (cdr (assoc :checksum old))
								 (format nil "~a"
									 (migration-checksum new-migration)))
						   (error "Migration ~a has a checksum error (checksum from on-disk ~a)." old (migration-checksum new-migration)))

						 (not (cdr (assoc :installed-successfully old))))
					       t))
					   

					   migrations)))
    (dolist (migration migrations-to-run)
      (with-slots (name checksum) migration
	(execute "INSERT INTO pichunter.migrations_tracker (filename, checksum) VALUES ($1, $2) ON CONFLICT DO NOTHING"
		 name
		 checksum)
	(exec-all (migration-code migration))
	(execute "UPDATE pichunter.migrations_tracker SET installed_successfully = TRUE where filename = $1" name)))))

(init-migration-system)
  
;; (with-db
;;     (clean)
;;     (migrate))
