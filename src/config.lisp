(defpackage :pichunter.config
  (:use :cl :pichunter.std)
  (:export :config))

(in-package :pichunter.config)

(defparameter config (handler-case (read-from-string (slurp #P"/etc/pichunter/config.lisp"))
		       (error (e) (format t "Error ~a when reading config, config and integrations are probably unavailble" e))))

