(asdf:defsystem #:sexp-config
  :description "Bounded fail-closed reading of s-expression configuration sources"
  :author "Lukáš Hozda"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on ()
  :components ((:module "source"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "grammar")
                             (:file "scan")
                             (:file "read"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:sexp-config/tests))))

(asdf:defsystem #:sexp-config/tests
  :description "Tests for sexp-config"
  :depends-on (#:sexp-config)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:sexp-config/tests '#:run-tests)))
