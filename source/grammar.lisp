(in-package #:sexp-config)

;;;; -- Source Grammar --

(defclass source-grammar ()
  ((label
    :initarg :label
    :reader source-grammar-label
    :type string
    :documentation "The phrase that opens every diagnostic, naming the source.")
   (keywords
    :initarg :keywords
    :reader source-grammar-keywords
    :type list
    :documentation "The accepted keyword designators, or NIL to accept any keyword.")
   (maximum-depth
    :initarg :maximum-depth
    :reader source-grammar-maximum-depth
    :type (integer 0)
    :documentation "The maximum accepted structural depth; 0 accepts no nesting.")
   (maximum-nodes
    :initarg :maximum-nodes
    :reader source-grammar-maximum-nodes
    :type (integer 0)
    :documentation "The maximum accepted count of conses and atoms.")
   (maximum-string-characters
    :initarg :maximum-string-characters
    :reader source-grammar-maximum-string-characters
    :type (or null (integer 0))
    :documentation "The maximum accepted string length, or NIL for unbounded.")
   (allowed-atom-predicate
    :initarg :allowed-atom-predicate
    :reader source-grammar-allowed-atom-predicate
    :type (or null function)
    :documentation "A predicate every atom must satisfy, or NIL to accept any atom.")
   (block-comments-permitted-p
    :initarg :block-comments-permitted-p
    :reader source-grammar-block-comments-permitted-p
    :type boolean
    :documentation "Whether nested block comments are accepted rather than rejected.")
   (improper-lists-permitted-p
    :initarg :improper-lists-permitted-p
    :reader source-grammar-improper-lists-permitted-p
    :type boolean
    :documentation "Whether a dotted list is accepted rather than rejected.")
   (list-tails-increase-depth-p
    :initarg :list-tails-increase-depth-p
    :reader source-grammar-list-tails-increase-depth-p
    :type boolean
    :documentation "Whether list length counts toward depth as well as nesting.")
   (shared-strings-permitted-p
    :initarg :shared-strings-permitted-p
    :reader source-grammar-shared-strings-permitted-p
    :type boolean
    :documentation "Whether one string object may appear at more than one place.")
   (common-lisp-symbols-permitted-p
    :initarg :common-lisp-symbols-permitted-p
    :reader source-grammar-common-lisp-symbols-permitted-p
    :type boolean
    :documentation "Whether the reader package inherits COMMON-LISP symbols."))
  (:documentation "The bounded data dialect one configuration source may use."))

(defun make-source-grammar
    (&key
       (label "the configuration source")
       keywords
       (maximum-depth 32)
       (maximum-nodes 4096)
       maximum-string-characters
       allowed-atom-predicate
       block-comments-permitted-p
       improper-lists-permitted-p
       list-tails-increase-depth-p
       (shared-strings-permitted-p t)
       (common-lisp-symbols-permitted-p t))
  "Create one validated source grammar.

LABEL opens every diagnostic, so give it a noun phrase such as \"SKILL.sexp\".
KEYWORDS whitelists the keyword tokens the source may name; NIL accepts any
keyword. MAXIMUM-DEPTH and MAXIMUM-NODES bound the structure, and either may be
zero to refuse every nested or non-empty source. MAXIMUM-STRING-CHARACTERS
bounds one string, which MAXIMUM-NODES cannot do because a string counts as one
node however long it is.

ALLOWED-ATOM-PREDICATE restricts which atoms may appear, which is how a caller
excludes symbols it never wants to interpret. IMPROPER-LISTS-PERMITTED-P accepts
dotted lists. LIST-TAILS-INCREASE-DEPTH-P also charges list length to the depth
budget, which makes MAXIMUM-DEPTH bound list length too.

BLOCK-COMMENTS-PERMITTED-P accepts nested #| |# comments. It is off by default
because a grammar that refuses the dispatch character cannot express them.

SHARED-STRINGS-PERMITTED-P may be set false to refuse one string object that
appears at two places. Nothing can produce that while the dispatch character is
refused, since reader labels need it, so this is defence in depth rather than a
parsing rule.

COMMON-LISP-SYMBOLS-PERMITTED-P controls whether the throwaway reader package
inherits COMMON-LISP. Leave it true unless the grammar has no place for a bare
symbol: with it false, the tokens NIL and T read as fresh uninterned symbols
rather than as the standard objects they name."
  (unless (stringp label)
    (error 'sexp-config-error
           :kind :invalid-grammar
           :label "the source grammar"
           :message "A source grammar label must be a string."))
  (unless (and (typep maximum-depth '(integer 0))
               (typep maximum-nodes '(integer 0)))
    (error 'sexp-config-error
           :kind :invalid-grammar
           :label label
           :message "Source grammar bounds must be non-negative integers."))
  (unless (or (null maximum-string-characters)
              (typep maximum-string-characters '(integer 0)))
    (error 'sexp-config-error
           :kind :invalid-grammar
           :label label
           :message "A source grammar string bound must be NIL or non-negative."))
  (unless (every #'symbolp keywords)
    (error 'sexp-config-error
           :kind :invalid-grammar
           :label label
           :message "Source grammar keywords must be symbols."))
  (make-instance 'source-grammar
                 :label label
                 :keywords (copy-list keywords)
                 :maximum-depth maximum-depth
                 :maximum-nodes maximum-nodes
                 :maximum-string-characters maximum-string-characters
                 :allowed-atom-predicate allowed-atom-predicate
                 :block-comments-permitted-p
                 (not (null block-comments-permitted-p))
                 :improper-lists-permitted-p
                 (not (null improper-lists-permitted-p))
                 :list-tails-increase-depth-p
                 (not (null list-tails-increase-depth-p))
                 :shared-strings-permitted-p
                 (not (null shared-strings-permitted-p))
                 :common-lisp-symbols-permitted-p
                 (not (null common-lisp-symbols-permitted-p))))

(defun grammar--fail (grammar kind control &rest arguments)
  "Signal a SEXP-CONFIG-ERROR of KIND describing GRAMMAR's rejected source."
  (error 'sexp-config-error
         :kind kind
         :label (source-grammar-label grammar)
         :message (apply #'format nil control arguments)))

(defun grammar--source-line (source offset)
  "Return the one-based line of SOURCE containing OFFSET."
  (1+ (count #\Newline source :end (min offset (length source)))))

(defun grammar--fail-at (grammar kind source offset control &rest arguments)
  "Signal a SEXP-CONFIG-ERROR of KIND located at OFFSET within SOURCE."
  (error 'sexp-config-error
         :kind kind
         :label (source-grammar-label grammar)
         :offset offset
         :line (grammar--source-line source offset)
         :message (apply #'format nil control arguments)))

(defun grammar--fail-token
    (grammar kind source offset token control &rest arguments)
  "Signal a SEXP-CONFIG-ERROR of KIND naming TOKEN found at OFFSET in SOURCE."
  (error 'sexp-config-error
         :kind kind
         :label (source-grammar-label grammar)
         :offset offset
         :line (grammar--source-line source offset)
         :token token
         :message (apply #'format nil control arguments)))
