(defpackage :pichunter.config
  (:use :cl :pichunter.std)
  (:export :config))

(in-package :pichunter.config)

(defun config (param)
  (getf 
   (read-from-string (slurp #P"/etc/pichunter/config.lisp"))
   param))
