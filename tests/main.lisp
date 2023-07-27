(defpackage pichunter/tests/main
  (:use :cl
        :postmodern
	:binding-arrows
	:pichunter.std
        :rove)
    (:shadowing-import-from :cl-strings :replace-all))
(in-package :pichunter/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :pichunter)' in your Lisp.


(deftest nulltest
    (testing "if tests are ran"
	     (ok (equalp 1 1))
	     (ok (string= "lol" "lol"))))

;; (defun init-db ()
;;   (execute "DROP SCHEMA IF EXISTS pichunter_test CASCADE")
;;   (execute "CREATE SCHEMA IF NOT EXISTS pichunter_test"))
