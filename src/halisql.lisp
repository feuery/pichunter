(defpackage :halisql
  (:use :cl :binding-arrows)
  (:import-from :pichunter.std :repeatedly :drop :partial :slurp-utf-8 :compose)
  (:export :*log*))

(in-package :halisql)

(defun slurp-sql (file-path)
  "Reads an sql-file into a compile-time constant you can push to exec-all"  
  (unless (or *load-pathname* *compile-file-pathname*)
    (format t "*load-pathname* and *compile-file-pathname* are nil, please don't C-c C-c defqueries macro but load them by C-c C-k'ing the current namespace"))
  (let* ((local-path (drop 1 (pathname-directory (or *load-pathname*
						     *compile-file-pathname*
						     #P"/Users/feuer/Projects/pichunter/src/"))))
	 (local-path (if (not (equalp (first (last local-path))
				      "src"))
			 (concatenate 'list local-path (list "src"))
			 local-path))
	 (file-path (pathname (format nil "/~{~a/~}~a"
						local-path
						file-path))))
    (slurp-utf-8 file-path)))

(defparameter interesting-keywords (list "name:" "returns:"))

(defun is-comment? (line)
  (str:starts-with? "--" line))

(defun merge-hash (hash1 hash2)
  (dolist (k (loop for k being each hash-key of hash2
		   collect k))
    (setf (gethash k hash1) (gethash k hash2)))
  hash1)

(defun query-meta (query)
  (let* ((relevant-lines (->> query
			   (remove-if-not #'is-comment?)
			   (mapcar (lambda (line)
				     (str:trim 
				      (str:replace-first "--" "" line))))))
	 (processed-records (->> relevant-lines
			      (remove-if-not
			       (lambda (line)
				 (some (lambda (kw)
					 (str:starts-with? kw line))
				       interesting-keywords)))
			      (mapcar (lambda (line)
					(let* ((result (str:split #\Space line ))
					       (key (str:replace-first ":" "" (first result)))
					       (value (rest result))
					       (modifiers (drop 2 result))
					       (dict (make-hash-table :size 2 :test 'equal)))
					  (setf (gethash key dict) value)
					  (when modifiers 
					    (setf (gethash "modifiers" dict) modifiers))
					  dict)))))
	 (result (make-hash-table :test 'equal)))

    (dolist (record processed-records)
      (setf result (merge-hash result record)))
    result))

(defun get-sql (query)
  (->> query
    (remove-if #'is-comment?)
    (str:join #\Newline)))

(defun amount-of-query-params (query-sql)
  (let ((params (->>
		  query-sql
		  (cl-ppcre:all-matches-as-strings "\\$\\d")
		  (mapcar (compose #'parse-integer (partial #'str:replace-all "$" ""))))))
    (if params
	(apply #'max params )
	0)))

(defparameter *log* nil)

(defmacro defqueries (file-path)
  (let ((queries (->> (uiop:split-string
		       (slurp-sql file-path)
		       :separator '(#\;))
		   (mapcar (lambda (fn)
			     (uiop:split-string fn :separator '(#\Newline))))
		   (remove-if (lambda (query)
				(every (partial #'string= "") query))))))
    `(progn
     ,@(->> queries
	 (mapcar (lambda (query)
		   (let* ((meta (query-meta query))
			  (sql (get-sql query))
			  (amount-of-params (amount-of-query-params sql))
			  (name (first (gethash "name" meta)))
			  (returns (str:join #\Space (gethash "returns" meta)))
			  (modifiers (gethash "modifiers" meta))
			  (execute? (some (partial #'string= "@execute") modifiers))
			  ;;(args (gensym))
			  (fn (if execute?
				  'postmodern:execute
				  'postmodern:query))
			  (params (loop for x from 1 to amount-of-params collect (gensym))))

		     `(defun ,(intern (string-upcase name)) ,params
			(when *log* 
			  (format t "running ~a~%" ,sql))
			(handler-case 
			    (,fn ,sql 
			      ,@params
			      ,(if (not (string= "" returns))
				 (let ((*read-eval* nil))
				   (format t "returns: ~a~%" (prin1-to-string returns))
				   (read-from-string (string-upcase returns)))
				 (if (equalp fn 'postmodern:query)
				     :rows
				     :none)))
			  (error (e)
			    (format t "caught error in ~a~%~a~%" (quote ,(intern (string-upcase name)))
				    e)
			    e)))))))
     '(,@(->> queries
	(mapcar (lambda (query)
		  (let ((meta (query-meta query)))
		    (intern (string-upcase (first (gethash "name" meta))))))))))))


;; (macroexpand-1  '
;;  (defqueries "user-routes.sql"))


;; (pichunter.std:with-db
;;     (postmodern:with-schema (:pichunter)

;; (do-something-stupid-with-users "feuer" "TESTI" (pichunter.std:sha-512 "passu")
;;   3
;;   (pichunter.std:sha-512 "passu"))))

