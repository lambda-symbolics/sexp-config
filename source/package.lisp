(defpackage #:sexp-config
  (:use #:cl)
  (:export #:make-source-grammar
           #:read-source
           #:read-source-file
           #:scan-source
           #:sexp-config-error
           #:sexp-config-error-kind
           #:sexp-config-error-label
           #:sexp-config-error-line
           #:sexp-config-error-message
           #:sexp-config-error-offset
           #:sexp-config-error-token
           #:source-grammar
           #:source-grammar-allowed-atom-predicate
           #:source-grammar-block-comments-permitted-p
           #:source-grammar-common-lisp-symbols-permitted-p
           #:source-grammar-improper-lists-permitted-p
           #:source-grammar-keyword-predicate
           #:source-grammar-keywords
           #:source-grammar-label
           #:source-grammar-list-tails-increase-depth-p
           #:source-grammar-maximum-depth
           #:source-grammar-maximum-nodes
           #:source-grammar-maximum-string-characters
           #:source-grammar-octet-vectors-permitted-p
           #:source-grammar-qualified-common-lisp-symbols-permitted-p
           #:source-grammar-read-default-float-format
           #:source-grammar-shared-strings-permitted-p
           #:validate-tree))

(defpackage #:sexp-config/tests
  (:use #:cl)
  (:import-from #:sexp-config
                #:make-source-grammar
                #:read-source
                #:read-source-file
                #:scan-source
                #:sexp-config-error
                #:sexp-config-error-kind
                #:sexp-config-error-line
                #:sexp-config-error-offset
                #:sexp-config-error-token
                #:validate-tree)
  (:export #:run-tests))
