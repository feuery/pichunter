(defsystem "pichunter"
  :version "0.1.0"
  :author "Ilpo Lehtinen"
  :license "LLGPL"
  :depends-on ("postmodern"
               "simple-date"
               "simple-date/postgres-glue"
	       "ironclad"
	       "binding-arrows"
	       "hunchentoot"
	       "com.inuoe.jzon"
	       "easy-routes"
	       "zpb-exif"
               "str")
  :components ((:module "src"
                :components
                ((:file "std")
		 (:file "migrations")
		 (:file "migration-list")
		 (:file "decorators")
		 (:file "user-routes")
		 (:file "file-handler")
		 (:file "media-routes")
		 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "pichunter/tests"))))

(defsystem "pichunter/tests"
  :author "Ilpo Lehtinen"
  :license "LLGPL"
  :depends-on ("pichunter"
	       "cl-strings"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for pichunter"
  :perform (test-op (op c) (symbol-call :rove :run c)))
