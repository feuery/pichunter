(defpackage :pichunter.config
  (:use :cl :pichunter.std)
  (:export :config))

(in-package :pichunter.config)

(defparameter config (read-from-string (slurp #P"/etc/pichunter/config.lisp")))
(assert config)
