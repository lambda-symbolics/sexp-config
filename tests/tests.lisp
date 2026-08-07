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
  (let ((grammar (make-source-grammar
                  :label "PREDICATE.sexp"
                  :keywords '(:config)
                  :keyword-predicate (lambda (name)
                                       (string-equal name "computed")))))
    (test-assert (equal (read-source "(:config :computed 1)" grammar)
                        '(:config :computed 1))
                 "a keyword predicate accepts a computed vocabulary")
    (test-assert (eq (rejection-kind
                      (lambda () (read-source "(:config :other 1)" grammar)))
                     :unknown-field)
                 "a keyword predicate still refuses what it does not accept"))
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

(defun tests--block-comments ()
  "Exercise nested block comments and their absence."
  (let ((grammar (tests--grammar :block-comments-permitted-p t)))
    (test-assert (equal (read-source "#| skipped |# (:config :version 1)" grammar)
                        '(:config :version 1))
                 "a block comment is skipped when the grammar permits it")
    (test-assert (equal (read-source "(:config #| a #| nested |# b |# :version 1)"
                                     grammar)
                        '(:config :version 1))
                 "nested block comments are skipped to the matching close")
    (test-assert (equal (read-source "(:config :name #|c|#\"x\")" grammar)
                        '(:config :name "x"))
                 "a keyword may follow a closed block comment")
    (test-assert (eq (rejection-kind
                      (lambda () (read-source "(:config #| unterminated" grammar)))
                     :invalid-syntax)
                 "an unterminated block comment is refused")
    (test-assert (equal (read-source "(:config :name \"#| not a comment |#\")"
                                     grammar)
                        '(:config :name "#| not a comment |#"))
                 "a block comment inside a string is text")
    (test-assert (equal (read-source "(:config :name \"a\") ; #| unclosed"
                                     grammar)
                        '(:config :name "a"))
                 "a block comment opened inside a line comment is text"))
  (test-assert (eq (rejection-kind
                    (lambda () (read-source "#| skipped |# (:config)"
                                            (tests--grammar))))
                   :invalid-syntax)
               "a block comment is refused by default")
  nil)

(defun tests--positions ()
  "Exercise the offset and line a rejection reports."
  (let ((grammar (tests--grammar)))
    (flet ((line-of (source)
             (handler-case
                 (progn (read-source source grammar) nil)
               (sexp-config-error (condition)
                 (sexp-config-error-line condition)))))
      (test-assert (= (line-of "(:config
                                :version 1
                                :unexpected 2)")
                      3)
                   "an unknown keyword reports the line it appears on")
      (test-assert (= (line-of "(:config
                                :version '1)")
                      2)
                   "unsupported syntax reports the line it appears on")
      (test-assert (= (line-of "(:config)
(:config)")
                      2)
                   "a second top-level form reports its own line")
      (test-assert (= (line-of "(:config :unexpected 1)") 1)
                   "a first-line rejection reports line one")))
  (let ((grammar (tests--grammar)))
    (test-assert
     (handler-case
         (progn (read-source "(:config
                              :unexpected 1)"
                             grammar)
                nil)
       (sexp-config-error (condition)
         (and (string= (sexp-config-error-token condition) ":unexpected")
              (integerp (sexp-config-error-offset condition)))))
     "a rejected token is reported with its offset"))
  nil)

(defun tests--string-bounds ()
  "Exercise the string length bound and shared string refusal."
  (let ((grammar (tests--grammar :maximum-string-characters 3)))
    (test-assert (equal (read-source "(:config :name \"abc\")" grammar)
                        '(:config :name "abc"))
                 "a string at the bound is accepted")
    (test-assert (eq (rejection-kind
                      (lambda () (read-source "(:config :name \"abcd\")" grammar)))
                     :data-too-large)
                 "a string past the bound is refused"))
  (let ((shared (copy-seq "same"))
        (grammar (tests--grammar :shared-strings-permitted-p nil)))
    (test-assert (eq (rejection-kind
                      (lambda () (validate-tree (list shared shared) grammar)))
                     :invalid-structure)
                 "a shared string is refused when the grammar forbids it")
    (test-assert (null (rejection-kind
                        (lambda () (validate-tree (list shared shared)
                                                  (tests--grammar)))))
                 "a shared string is accepted by default"))
  nil)

(defun tests--grammar-validation ()
  "Exercise rejection of malformed grammars."
  (test-assert (eq (rejection-kind (lambda () (make-source-grammar :label 42)))
                   :invalid-grammar)
               "a non-string label is refused")
  (test-assert (eq (rejection-kind
                    (lambda () (make-source-grammar :maximum-depth -1)))
                   :invalid-grammar)
               "a negative bound is refused")
  (test-assert (eq (rejection-kind
                    (lambda () (scan-source "(:config)"
                                            (tests--grammar :maximum-depth 0))))
                   :data-too-deep)
               "a zero depth bound refuses every parenthesized source")
  (test-assert (eq (rejection-kind
                    (lambda () (make-source-grammar :keywords '("name"))))
                   :invalid-grammar)
               "non-symbol keywords are refused")
  (test-assert (eq (rejection-kind
                    (lambda ()
                      (make-source-grammar :maximum-string-characters -1)))
                   :invalid-grammar)
               "a negative string bound is refused")
  nil)

(defun run-tests ()
  "Run every sexp-config regression test."
  (setf *test-count* 0)
  (tests--scanning)
  (tests--structure)
  (tests--reader-isolation)
  (tests--block-comments)
  (tests--positions)
  (tests--string-bounds)
  (tests--grammar-validation)
  (format t "~&~:D sexp-config tests passed.~%" *test-count*)
  nil)
