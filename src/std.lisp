(defpackage pichunter.std
  (:use :cl)
  (:import-from :postmodern :with-connection)
  (:export :hash-keys :slurp :if-let :when-let :with-db :take :slurp-string-body :sha-512 :encode-to-json)
  (:import-from :json-mop :encode))

(in-package pichunter.std)

(defmacro with-db (&rest body)
  `(with-connection '("pichunter" "pichunter" "TESTIPASSU" "localhost" :pooled-p t)
     ,@body))

(defun slurp (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun slurp-string-body (raw-body)
  ;; read-line does something completely moronic that makes valid-utf-8 look bad on the lisp side
  (trivial-utf-8:utf-8-bytes-to-string 
   (loop for byte = (read-byte raw-body nil)
	 while byte
	 collect byte)))

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
  "Returns a lazy sequence of the first n items from the given list lst."
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

(defun encode-to-json (obj)
  (with-output-to-string (s)
    (encode obj s)))
