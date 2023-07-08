(defpackage :pichunter.defclass
  (:use :cl)
  (:import-from :binding-arrows :->>)
  (:import-from :pichunter.std :when-let :if-let)
  (:export :when-let :compose :obj->alist :defclass* :with-slots* :validators :events :alist->obj))

(in-package :pichunter.defclass)

(defun drop (list n)
  (if (> n 0)
      (drop (cdr list)
	    (1- n))
      list))

(defun drop-even-indexes (list &optional acc) nil
  (if list
      (drop-even-indexes (cddr list) (cons (car list) acc))
      (reverse acc)))

(defun create-symbol (sym)
  (assert (stringp sym))
  (read-from-string sym))

(defun str (&rest strs)
  (apply (partial #'concatenate 'string)
	 (mapcar (lambda (o)
		   (write-to-string o :escape nil))
		 strs)))

(defun take (list n)
  (labels ((-take (list n acc)
	     (if (and (car list)
		      (> n 0))
		 (-take (cdr list)
			(1- n)
			(cons (car list) acc))
		 acc)))
    (reverse (-take list n '()))))

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v)) 
                rest
                :initial-value (apply fn1 args)))))

(defun partial (f &rest args)
  (lambda (&rest rst-args)
    (apply f (concatenate 'list args rst-args))))

(defun walk-and-transform (predicate transform tree &key (transform-lists nil))
  (cond ((hash-table-p tree)
	 (walk-and-transform predicate transform (fset:convert 'fset:map tree) :transform-lists transform-lists))
	((fset:seq? tree)
	 (walk-and-transform predicate transform  (fset:convert 'list tree) :transform-lists transform-lists))
	;; tää listahaarakin vois vaatia jotain predicate/transform juttua
	((listp tree)
	 (if transform-lists
	     (let ((result (if (funcall predicate tree)
			       (funcall transform tree)
			       tree)))
	       (if (listp result)
		   (mapcar (lambda (tr) (walk-and-transform predicate transform tr :transform-lists transform-lists)) result)
		   result))
	     (mapcar (lambda (tr) (walk-and-transform predicate transform tr :transform-lists transform-lists)) tree)))
	((fset:map? tree)
	 ;; Do we want to handle maps as leaves of the tree?
	 (if (funcall predicate tree)
	     (funcall transform tree)
	     (fset:image (lambda (k v)			   
			   (values k
				   ;; lets handle the actual atoms of the tree
				   (if (funcall predicate v)
				       (funcall transform v)
				       (walk-and-transform predicate transform v :transform-lists transform-lists))))
			 tree)))
	(t tree)))

(defun fn-list? (fn)
  (or (functionp fn)
      (and (listp fn)
	   (or (equalp (first fn) 'lambda)
	       (equalp (first fn) 'function))
	   (functionp (eval fn)))))

(defun any? (fn coll)
  (some fn coll))

(defun get-setf-destinations (body)
  (let ((dsts nil))
    (walk-and-transform (lambda (f)
			  (and (listp f)
			       (equalp (first f) 'setf)))
			(lambda (f)
			  (push (drop-even-indexes (rest f)) dsts )
			  f)
			body :transform-lists t)
    (apply #'concatenate 'list dsts)))

(defparameter validators (fset:empty-map))
(defparameter events (fset:empty-map))
(defparameter nonserializables (fset:empty-map))
(defvar class->props (fset:empty-map))

(defmacro defclass* (class-name &rest slot-val-tags-triplets)
  (unless (car slot-val-tags-triplets)
    (format t "First slot is nil. Have you converted from the clos defclass perhaps?~%")
    (error 'error ))
  
  (let* ((ctr-name (create-symbol (str "make-" (symbol-name class-name))))
	 (slots (mapcar #'first slot-val-tags-triplets))
	 (slot-val-pairs (mapcar (lambda (l) (take l 2)) slot-val-tags-triplets))
	 (ctr-params `(&key ,@slot-val-pairs))
	 (getters (->> slots
		       (mapcar (lambda (slot)
				 (list slot
				       (create-symbol (str (symbol-name class-name) "-" (symbol-name slot))))))
		       (mapcar (lambda (slot-getter-name)
				 (destructuring-bind (slot getter-name) slot-getter-name
				   `(progn
				      (defun ,getter-name (obj)
					(fset:lookup obj (symbol-name ',slot)))
				      ;; (export ',getter-name)
				      ))))))
	 (non-serializables (->> slot-val-tags-triplets
				 (remove-if-not (lambda (tripl)
						  (let ((tags (first (remove-if-not #'listp (drop tripl 2)))))
						    (when (and tags
							       (every #'symbolp tags))
						      (position 'nonserializable tags :test #'string=)))))
				 (mapcar (compose #'prin1-to-string #'first))
				 (cons 'list)))
	 (slot->validator (reduce (lambda (map slot-triplet)
				    (let ((slot-name (first slot-triplet))
					  (validator (first (remove-if-not #'fn-list? slot-triplet))))
				      (fset:with map (prin1-to-string slot-name) (eval validator))))
				    
				  (remove-if-not (lambda (tripl)
						   (any? #'fn-list? 
							 (last tripl 2)))
						 slot-val-tags-triplets)
				  :initial-value (fset:empty-map))))
    (setf class->props (fset:with class->props (symbol-name class-name) slots))
    (unless (fset:empty? slot->validator)
      (setf validators (fset:with validators (prin1-to-string class-name) slot->validator)))
    `(progn
       (setf nonserializables (fset:with nonserializables (prin1-to-string ',class-name) ,non-serializables))
       ,@getters
       (defun ,ctr-name ,ctr-params
	 (fset:map
	  ("TYPE" ,(prin1-to-string class-name))
	  ("NONSERIALIZABLES" ,non-serializables)
	  ,@(mapcar (lambda (slot-val)
		      (list (prin1-to-string slot-val) slot-val))
		    slots)))
       ;; sanity check that makes sure reflection towards emacs api works
       (assert (fset:lookup class->props ,(prin1-to-string class-name))))))

(defmethod fset:lookup ((collection hash-table) key)
  (gethash key collection :not-found))

(defmethod fset:compare ((x symbol) (y symbol))
  (if (string= x y)
      :equal
      :unequal))

(defmacro with-slots* (slots obj &rest body)
  "with-slots lookalike that lets you setf fields inside immutable fset maps. It has some issues with composing multiple with-slots*'s. Macro returns a new fset map with modified keys, unless first form of body is equalp to :read-only, in which case it returns whatever body returns"
  (let ((obj-sym (if (symbolp obj)
		     obj
		     (gensym)))
	(type-sym (gensym))
	(validators-sym (gensym))
	(events-sym (gensym))
	(slot-name (gensym))
	(event-lambda (gensym))
	(read-only? (equalp (car body) :read-only))
	(force-convert-to-fset? (equalp (car body) :force-fset)))
    (if read-only?
	`(let ((,obj-sym (if (hash-table-p ,obj)
			     ,obj
			     (fset:convert 'hash-table ,obj :test #'equalp))))
	   ,@(reduce (lambda (body slot)
		       (subst `(gethash ,(symbol-name slot) ,obj-sym)
			      slot
			      body))
		     slots
		     :initial-value body))

	
	`(let* ((,obj-sym (if (hash-table-p ,obj)
			      ,obj
			      (fset:convert 'hash-table ,obj :test #'equalp)))
		(setf-destinations '(,@(get-setf-destinations body)))
		(,type-sym (gethash "TYPE" ,obj-sym))
		(,validators-sym (fset:lookup pichunter.defclass:validators ,type-sym))
		(,events-sym (fset:lookup pichunter.defclass:events ,type-sym)))
	   ,@(reduce (lambda (body slot)
		       (->> body						       
			 (subst `(gethash ,(symbol-name slot) ,obj-sym)
				slot)))
		     slots
		     :initial-value body)
	   (when ,validators-sym
	     (fset:do-map (prop-name val (fset:convert 'map ,obj-sym))
	       (pichunter.std:when-let (validator (fset:lookup ,validators-sym prop-name))
		 (if (not (funcall validator val))
		     (error 'validator-failed)))))
	   (if ,events-sym
	       (fset:do-map (,slot-name ,event-lambda ,events-sym)
		 (if (position ,slot-name setf-destinations :test #'string=)
		     (progn
					;(format t "Event found! ~%")
		       (funcall ,event-lambda ,obj-sym ,slot-name (fset:lookup ,obj-sym ,slot-name)))
		     ;; (format t "slot ~a not found from ~a ~%" ,slot-name setf-destinations)
		     ))
	       ;; (format t "No events found for class ~a~%" ,type-sym)
	       )
	   
	   
	   (if (or (hash-table-p ,obj-sym)
		   ,force-convert-to-fset?)
	       (fset:convert 'fset:map ,obj-sym)
	       ,obj-sym)))))

(defun alist->obj (expected-type alist)
  (fset:with
   (fset:with (->> alist
		(mapcar (lambda (cell)
			  (destructuring-bind (k . v) cell
			    (if (stringp k)
				(cons k v)
				(cons (symbol-name k) v)))))
		(fset:convert 'fset:map))
	      "TYPE" (prin1-to-string expected-type))
   "NONSERIALIZABLES" (fset:lookup nonserializables (prin1-to-string expected-type))))
  

(defun obj->alist (obj)

  (if (fset:map? obj)
      (let* ((nonserializable-keys
	       (fset:lookup obj "NONSERIALIZABLES")))
	(->> (fset:less obj "NONSERIALIZABLES")
	  (fset:convert 'list)
	  (remove-if (lambda (cons)
		       (let ((key (car cons)))
			 (member key nonserializable-keys :test #'string=))))
	  (mapcar (lambda (element)
		    (destructuring-bind (key . value) element
		      (cons key (cond
				  ((listp value) (mapcar #'obj->alist value))
				  ((fset:map? value) (obj->alist value))
				  (t value))))))))
      obj))


				       
