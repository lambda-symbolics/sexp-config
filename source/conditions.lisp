(in-package #:sexp-config)

;;;; -- Conditions --

(define-condition sexp-config-error (error)
  ((kind
    :initarg :kind
    :reader sexp-config-error-kind
    :type keyword
    :documentation "The machine-readable diagnostic kind.")
   (message
    :initarg :message
    :reader sexp-config-error-message
    :type string
    :documentation "The human-readable description of the rejection.")
   (label
    :initarg :label
    :reader sexp-config-error-label
    :type string
    :documentation "The grammar label naming the rejected source.")
   (token
    :initarg :token
    :initform nil
    :reader sexp-config-error-token
    :type (or null string)
    :documentation "The offending token, when one identifies the rejection.")
   (offset
    :initarg :offset
    :initform nil
    :reader sexp-config-error-offset
    :type (or null (integer 0))
    :documentation "The zero-based character offset of the rejection, when known.")
   (line
    :initarg :line
    :initform nil
    :reader sexp-config-error-line
    :type (or null (integer 1))
    :documentation "The one-based source line of the rejection, when known."))
  (:report
   (lambda (condition stream)
     (write-string (sexp-config-error-message condition) stream)))
  (:documentation "One configuration source violates its declared grammar.

Hosts that keep their own diagnostic conditions translate this at the boundary
rather than passing it to their users. KIND is stable and suitable for that
translation."))
