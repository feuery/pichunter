(defpackage pichunter.std
  (:use :cl)
  (:import-from :postmodern :with-connection)
  (:export :in-compile-path :repeatedly :drop :hash-keys :slurp :slurp-bytes :partial :if-let :when-let :with-db :take :sha-512 :slurp-utf-8 :*automatic-tests-mode* :*test-position-lat* :*test-position-lng* :compose :e2e?))

(in-package pichunter.std)

(defvar *automatic-tests-mode* nil)
(defvar *test-position-lat* nil)
(defvar *test-position-lng* nil)

(defmacro with-db (&rest body)
  `(let ((db (or (sb-ext:posix-getenv "PICHUNTER_DB")
		 "pichunter"))
	 (username (or (sb-ext:posix-getenv "PICHUNTER_DB_USER")
		       "pichunter"))
	 (password (or (sb-ext:posix-getenv "PICHUNTER_DB_PASSWD")
		       "TESTIPASSU"))
	 (host (or (sb-ext:posix-getenv "PICHUNTER_DB_HOST")
		   "localhost"))
	 (port (let ((port-str (sb-ext:posix-getenv "PICHUNTER_DB_PORT")))
		 (if port-str
		     (parse-integer port-str)
		     5432))))
     (format t "database settings: ~a" (list db username password host
					     :port port
					     :pooled-p t))
     (with-connection `(,db ,username ,password ,host
			    :port ,port
			    :pooled-p t)
       ,@body)))

;;(macroexpand-1 '(with-db (format t "lol")))

;; https://www.n16f.net/blog/reading-files-faster-in-common-lisp/
(defun slurp-bytes (path)
  (declare (type (or pathname string) path))
  (let ((data (make-array 0 :element-type '(unsigned-byte 8) :adjustable t))
        (block-size 4096)
        (offset 0))
    (with-open-file (file path :element-type '(unsigned-byte 8))
      (loop
        (let* ((capacity (array-total-size data))
               (nb-left (- capacity offset)))
          (when (< nb-left block-size)
            (let ((new-length (max (+ capacity (- block-size nb-left))
                                   (floor (* capacity 3) 2))))
              (setf data (adjust-array data new-length)))))
        (let ((end (read-sequence data file :start offset)))
          (when (= end offset)
            (return-from slurp-bytes (adjust-array data end)))
          (setf offset end))))))

(defun slurp-utf-8 (path)
  (trivial-utf-8:utf-8-bytes-to-string (slurp-bytes path)))

(defun slurp (path &key (external-format :default))
  (with-open-file (stream path :external-format external-format)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defmacro when-let (bindings &rest body)
  (let ((symbol (first bindings))
	(initform (second bindings)))
    `(let ((,symbol ,initform))
       (when ,symbol
	 ,@body))))


(defmacro if-let (bindings true-body false-body)
  (let ((symbol (first bindings))
	(initform (second bindings)))
    `(let ((,symbol ,initform))
       (if ,symbol
	   ,true-body
	   ,false-body))))

(defun take (n lst &optional acc)
  "Returns a sequence of the first n items from the given list lst."
  (labels ((take-tailrec (n lst acc)
             (if (or (zerop n) (endp lst))
                 (reverse acc)
                 (take-tailrec (1- n) (cdr lst) (cons (car lst) acc)))))
    (take-tailrec n lst '())))

(defmacro while (condition &body body)
  `(loop while ,condition do ,@body))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun sha-512 (str)
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :sha512
                              (ironclad:ascii-string-to-byte-array str))))

(defun drop (n lst)
  "Returns a sequence that skips the first N elements of the given list."
  (cond ((or (null lst) (<= n 0)) lst)
        ((> n 0) (drop (1- n) (cdr lst)))))


(defun partial (f &rest args)
  (lambda (&rest rst-args)
    (apply f (concatenate 'list args rst-args))))

(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))

(defun repeatedly (fn n)
  (if (equalp n 0)
      '()
      (cons (funcall fn n) (repeatedly fn (1- n)))))

(defun e2e? ()
  (sb-ext:posix-getenv "PICHUNTER_E2E"))

(defmacro in-compile-path (file-path)
  ;; (unless (or *load-pathname* *compile-file-pathname*)
  ;;   (format t "*load-pathname* and *compile-file-pathname* are nil, please don't C-c C-c in-compile-path macro but load them by C-c C-k'ing the current namespace"))

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
    file-path))
