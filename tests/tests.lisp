(in-package #:sexp-config/tests)

(defun tests--grammar (&rest arguments)
  "Return a test grammar accepting a small keyword vocabulary."
  (apply #'make-source-grammar
         :label "TEST.sexp"
         :keywords '(:config :version :name :values :enabled)
         arguments))

(defparameter *rejected-sources*
  '((:invalid-syntax  "(:config :version #xFF)")
    (:invalid-syntax  "(:config :version '1)")
    (:invalid-syntax  "(:config :version `1)")
    (:invalid-syntax  "(:config :name |odd|)")
    (:invalid-syntax  "(:config :name a\\b)")
    (:invalid-syntax  "(:config :name cl:car)")
    (:invalid-syntax  "(:config :name \"unterminated)")
    (:invalid-syntax  "(:config :name \"x\"))")
    (:invalid-syntax  "(:config :name \"x\"")
    (:unknown-field   "(:config :unexpected 1)")
    (:no-form         "   ; only a comment")
    (:multiple-forms  "(:config) (:config)"))
  "Sources every grammar in these tests must refuse, with the expected kind.")

(defun tests--scanning ()
  "Exercise the pre-reader scan across unsupported syntax and vocabulary."
  (let ((grammar (tests--grammar)))
    (dolist (case *rejected-sources*)
      (let ((kind (rejection-kind
                   (lambda () (read-source (second case) grammar)))))
        (test-assert (eq kind (first case))
                     (format nil "~S is refused as ~S, got ~S"
                             (second case) (first case) kind))))
    (test-assert (equal (read-source "(:config :version 1)" grammar)
                        '(:config :version 1))
                 "an accepted source reads as data")
    (test-assert (equal (read-source "; leading comment
                                      (:config :name \"a#b'c\")"
                                     grammar)
                        '(:config :name "a#b'c"))
                 "comments are skipped and strings may hold refused characters")
    (test-assert (equal (read-source "(:config :values (1 2 3))" grammar)
                        '(:config :values (1 2 3)))
                 "nested lists read as data"))
  (test-assert (equal (read-source "(:anything 1)"
                                   (make-source-grammar :label "ANY.sexp"))
                      '(:anything 1))
               "a grammar without a keyword whitelist accepts any keyword")
  (test-assert (eq (rejection-kind
                    (lambda () (scan-source "((((:config))))"
                                            (tests--grammar :maximum-depth 3))))
                   :data-too-deep)
               "the scan rejects parentheses deeper than the grammar")
  nil)

(defun tests--structure ()
  "Exercise depth, node, dotted-list, and atom validation."
  (let ((grammar (tests--grammar :maximum-nodes 6)))
    (test-assert (eq (rejection-kind
                      (lambda () (validate-tree '(1 2 3 4 5 6 7) grammar)))
                     :data-too-large)
                 "the node limit rejects an oversized form"))
  (let ((grammar (tests--grammar)))
    (test-assert (eq (rejection-kind
                      (lambda () (validate-tree '(:config . :values) grammar)))
                     :invalid-structure)
                 "a dotted list is refused by default")
    (let ((shared (list 1 2)))
      (test-assert (eq (rejection-kind
                        (lambda () (validate-tree (list shared shared) grammar)))
                       :invalid-structure)
                   "shared list structure is always refused"))
    (let ((circular (list 1 2)))
      (setf (rest (last circular)) circular)
      (test-assert (eq (rejection-kind
                        (lambda () (validate-tree circular grammar)))
                       :invalid-structure)
                   "circular list structure is always refused")))
  (test-assert (equal (validate-tree '(:config . :values)
                                     (tests--grammar
                                      :improper-lists-permitted-p t))
                      '(:config . :values))
               "a grammar may permit dotted lists")
  (let ((grammar (tests--grammar :maximum-depth 4)))
    (test-assert (null (rejection-kind
                        (lambda () (validate-tree '(1 2 3 4 5 6) grammar))))
                 "list length does not count toward depth by default"))
  (let ((grammar (tests--grammar :maximum-depth 4
                                 :list-tails-increase-depth-p t)))
    (test-assert (eq (rejection-kind
                      (lambda () (validate-tree '(1 2 3 4 5 6) grammar)))
                     :data-too-deep)
                 "a grammar may charge list length to the depth budget"))
  (let ((grammar (tests--grammar
                  :allowed-atom-predicate
                  (lambda (value)
                    (or (null value) (keywordp value) (stringp value))))))
    (test-assert (eq (rejection-kind
                      (lambda () (validate-tree '(:config 1) grammar)))
                     :invalid-value)
                 "an atom predicate refuses values outside the grammar")
    (test-assert (null (rejection-kind
                        (lambda () (validate-tree '(:config "1" nil) grammar))))
                 "an atom predicate accepts values inside the grammar"))
  nil)

(defun tests--reader-isolation ()
  "Exercise reader safety and the throwaway reader package."
  (let ((grammar (tests--grammar)))
    (test-assert (eq (rejection-kind
                      (lambda () (read-source "(:config #.(error \"no\"))"
                                              grammar)))
                     :invalid-syntax)
                 "read-time evaluation is refused before the reader sees it")
    (let ((before (length (list-all-packages))))
      (read-source "(:config :version 1)" grammar)
      (read-source "(:config :version 1)" grammar)
      (test-assert (= (length (list-all-packages)) before)
                   "the throwaway reader package is deleted after each read"))
    (test-assert (eq (rejection-kind
                      (lambda () (read-source "(:config :name \"x\" . 1)"
                                              grammar)))
                     :invalid-structure)
                 "a dotted form read from source is refused structurally"))
  (let ((strict (tests--grammar :common-lisp-symbols-permitted-p nil))
        (permissive (tests--grammar)))
    (test-assert (null (second (read-source "(:config nil)" permissive)))
                 "an inherited COMMON-LISP package reads NIL as the empty list")
    (let ((value (second (read-source "(:config nil)" strict))))
      (test-assert (and (symbolp value) (not (null value)))
                   "a grammar without COMMON-LISP reads NIL as a fresh symbol")))
  nil)

(defun tests--grammar-validation ()
  "Exercise rejection of malformed grammars."
  (test-assert (eq (rejection-kind (lambda () (make-source-grammar :label 42)))
                   :invalid-grammar)
               "a non-string label is refused")
  (test-assert (eq (rejection-kind
                    (lambda () (make-source-grammar :maximum-depth 0)))
                   :invalid-grammar)
               "a non-positive bound is refused")
  (test-assert (eq (rejection-kind
                    (lambda () (make-source-grammar :keywords '("name"))))
                   :invalid-grammar)
               "non-symbol keywords are refused")
  nil)

(defun run-tests ()
  "Run every sexp-config regression test."
  (setf *test-count* 0)
  (tests--scanning)
  (tests--structure)
  (tests--reader-isolation)
  (tests--grammar-validation)
  (format t "~&~:D sexp-config tests passed.~%" *test-count*)
  nil)
