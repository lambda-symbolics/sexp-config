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
                             (:file "scan")))))
