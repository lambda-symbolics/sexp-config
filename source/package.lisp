(defpackage #:sexp-config
  (:use #:cl)
  (:export #:make-source-grammar
           #:read-source
           #:scan-source
           #:sexp-config-error
           #:sexp-config-error-kind
           #:sexp-config-error-label
           #:sexp-config-error-message
           #:sexp-config-error-token
           #:source-grammar
           #:source-grammar-allowed-atom-predicate
           #:source-grammar-common-lisp-symbols-permitted-p
           #:source-grammar-improper-lists-permitted-p
           #:source-grammar-keywords
           #:source-grammar-label
           #:source-grammar-list-tails-increase-depth-p
           #:source-grammar-maximum-depth
           #:source-grammar-maximum-nodes
           #:validate-tree))

(defpackage #:sexp-config/tests
  (:use #:cl)
  (:import-from #:sexp-config
                #:make-source-grammar
                #:read-source
                #:scan-source
                #:sexp-config-error
                #:sexp-config-error-kind
                #:sexp-config-error-token
                #:validate-tree)
  (:export #:run-tests))
