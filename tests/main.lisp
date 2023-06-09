(defpackage pichunter/tests/main
  (:use :cl
        :postmodern
	:binding-arrows
	:pichunter.std
        :pichunter.defclass
        :rove)
    (:shadowing-import-from :cl-strings :replace-all))
(in-package :pichunter/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :pichunter)' in your Lisp.

(defun set-prop (obj k v)
  (cond ((fset:map? obj)
	 (fset:with obj k v))
	((listp obj)
	 (setf (nth k obj) v)
	 obj)))

(defun get-prop (obj key)
  (cond ((fset:map? obj)
	 (fset:lookup obj key))
	((listp obj)
	 (nth key obj))
	((hash-table-p obj)
	 (gethash key obj))
	(t
	 (format t "~a is neither hashtable, list nor fset map~%" obj)
	 (assert (or (fset:map? obj)
		     (hash-table-p obj)
		     (listp obj))))))

(defun update-prop (obj key fn)
  (set-prop obj key (funcall fn (get-prop obj key))))


(deftest defclass*-testing
  (defclass* luokka
      (property "lol" )
    (a 23)
    (asd 666))

  (let ((obj (make-luokka :a (* 23 2))))
    (testing "if pichunter.defclass serialization is symmetric"
      (ok 
       (equalp obj
	       (alist->obj (let ((yason:*parse-object-as* :alist))
			     (yason:parse 
			      (with-output-to-string (s)
				(yason:encode-alist
				 (fset:convert 'list obj) s)))))))))
  (defclass* non-serializables
      (a "lol")
    (b "sdf")
    (ddd "#<something that's not going to serialize>" (nonserializable)))
  (testing "if obj->alist returns anything coherent"
    (let ((object (make-non-serializables :b 44 :a (list 1 2 3))))
      
      (ok (equalp
	   `(("A" 1 2 3) ("B" . 44) 
	     ("TYPE" . "NON-SERIALIZABLES"))
	   (obj->alist object))))
    
    (let* ((object (make-non-serializables :b 44 :a (list 1 2 3)))
	   (obj2 (make-non-serializables :a object :b "sdf")))
      (ok (equalp
	   `(("A" ("A" 1 2 3) ("B" . 44)
		  
		  ("TYPE" . "NON-SERIALIZABLES"))
	     ("B" . "sdf")
	     ("TYPE" . "NON-SERIALIZABLES"))
	   (obj->alist obj2)))))
  (testing "recursive alist->obj"
    (let* ((object (make-non-serializables :b 44 :a (list 1 2 3)))
	   (obj2 (make-non-serializables :a object :b "sdf")))
      (ok (equalp (-> obj2
		    (update-prop "A" (lambda (o) (fset:less o "DDD")))
		    (fset:less "DDD"))
		  (alist->obj (obj->alist obj2)))))))



(defun init-db ()
  (execute "DROP SCHEMA IF EXISTS pichunter_test CASCADE")
  (execute "CREATE SCHEMA IF NOT EXISTS pichunter_test"))

(deftest db-defclass-interaction-testing
  (with-db
      (init-db)
    (testing "if database is available at all"
      (execute (:create-table 'pichunter_test.test-object
			      ((id :type serial)
			       (name :type string :unique)
			       (age :type integer))))
      (execute (:insert-into 'pichunter_test.test-object
		:set 'name "Testi" 'age 66))

      (ok
       (equalp
	(query (:select :name :age
			:from 'pichunter_test.test-object))
	`(("Testi" 66)))))

    (testing "defclass* serialization"
      (defclass* test-object
	(name "")
	(age 0))
      (let ((instance (make-test-object :name "lollero olio"
					:age 666)))
	(with-slots* (name age) instance 
	  (execute (:insert-into 'pichunter_test.test-object
		    :set :name name :age age)))
	
	(let ((instance2 (alist->obj (query (:select :name :age
					     :from 'pichunter-test.test-object
					     :where (:= :age 666)) 
					    :alist)
				     :type 'test-object )))
	  (ok
	   (equalp instance
		   instance2)))))))
