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
   (allowed-atom-predicate
    :initarg :allowed-atom-predicate
    :reader source-grammar-allowed-atom-predicate
    :type (or null function)
    :documentation "A predicate every atom must satisfy, or NIL to accept any atom.")
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
       allowed-atom-predicate
       improper-lists-permitted-p
       list-tails-increase-depth-p
       (common-lisp-symbols-permitted-p t))
  "Create one validated source grammar.

LABEL opens every diagnostic, so give it a noun phrase such as \"SKILL.sexp\".
KEYWORDS whitelists the keyword tokens the source may name; NIL accepts any
keyword. MAXIMUM-DEPTH and MAXIMUM-NODES bound the structure, and either may be
zero to refuse every nested or non-empty source.

ALLOWED-ATOM-PREDICATE restricts which atoms may appear, which is how a caller
excludes symbols it never wants to interpret. IMPROPER-LISTS-PERMITTED-P accepts
dotted lists. LIST-TAILS-INCREASE-DEPTH-P also charges list length to the depth
budget, which makes MAXIMUM-DEPTH bound list length too.

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
                 :allowed-atom-predicate allowed-atom-predicate
                 :improper-lists-permitted-p
                 (not (null improper-lists-permitted-p))
                 :list-tails-increase-depth-p
                 (not (null list-tails-increase-depth-p))
                 :common-lisp-symbols-permitted-p
                 (not (null common-lisp-symbols-permitted-p))))

(defun grammar--fail (grammar kind control &rest arguments)
  "Signal a SEXP-CONFIG-ERROR of KIND describing GRAMMAR's rejected source."
  (error 'sexp-config-error
         :kind kind
         :label (source-grammar-label grammar)
         :message (apply #'format nil control arguments)))

(defun grammar--fail-token (grammar kind token control &rest arguments)
  "Signal a SEXP-CONFIG-ERROR of KIND naming the offending TOKEN."
  (error 'sexp-config-error
         :kind kind
         :label (source-grammar-label grammar)
         :token token
         :message (apply #'format nil control arguments)))
