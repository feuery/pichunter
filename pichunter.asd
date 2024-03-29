(defsystem "pichunter"
  :version "0.1.0"
  :author "Ilpo Lehtinen"
  :license "LLGPL"
  :depends-on ("postmodern"
               "simple-date"
               "simple-date/postgres-glue"
	       "ironclad"
	       "trivial-utf-8"
	       "binding-arrows"
	       "hunchentoot"
	       "cl-ppcre"
	       "cl-geocode"
	       "parse-number"
	       "com.inuoe.jzon"
	       "easy-routes"
	       "zpb-exif"
	       "drakma"
               "str"
	       "log4cl"
	       "cl-advice")
  :components ((:module "src"
                :components
                ((:file "std")
		 (:file "halisql")
		 (:file "config")
		 (:file "migrations")
		 (:file "migration-list")
		 (:file "user-class")
		 (:file "decorators")
		 (:file "user-routes")
		 (:file "file-handler")
		 (:file "media-routes")
		 (:file "game-routes")
		 (:file "e2e")
		 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "pichunter/tests"))))

(defsystem "pichunter/tests"
  :author "Ilpo Lehtinen"
  :license "LLGPL"
  :depends-on ("pichunter"
	       "cl-strings"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main")
		 (:file "game"))))
  :description "Test system for pichunter"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :my-system :pichunter/tests)))
  ;; :perform (test-op (op c) (symbol-call :rove :run c))
  )
