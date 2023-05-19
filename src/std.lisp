(defpackage pichunter.std
  (:use :cl)
  (:import-from :postmodern :with-connection)
  (:export :slurp :if-let :when-let :with-db))

(in-package pichunter.std)

(defmacro with-db (&rest body)
  `(with-connection '("pichunter" "pichunter" "TESTIPASSU" "localhost" :pooled-p t)
     ,@body))

(defun slurp (path)
  (with-open-file (stream path)
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
