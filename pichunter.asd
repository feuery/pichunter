(defsystem "pichunter"
  :version "0.1.0"
  :author "Ilpo Lehtinen"
  :license "LLGPL"
  :depends-on ("postmodern"
	       "cl-who"
               "simple-date"
               "simple-date/postgres-glue"
               "clack"
	       "http-body"
               "cl-json"
               "str")
  :components ((:module "src"
                :components
                ((:file "std")
		 (:file "migrations")
		 (:file "migration-list")
		 (:file "decorators")
		 (:file "file-handler")
		 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "pichunter/tests"))))

(defsystem "pichunter/tests"
  :author "Ilpo Lehtinen"
  :license "LLGPL"
  :depends-on ("pichunter"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for pichunter"
  :perform (test-op (op c) (symbol-call :rove :run c)))
