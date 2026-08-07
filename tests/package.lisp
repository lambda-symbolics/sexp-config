(in-package #:sexp-config/tests)

(defvar *test-count* 0
  "The number of assertions executed by the current test run.")

(defun test-assert (condition description)
  "Record and require CONDITION for DESCRIPTION."
  (incf *test-count*)
  (unless condition
    (error "sexp-config test failed: ~A" description))
  t)

(defun rejection-kind (thunk)
  "Return the diagnostic kind THUNK signals, or NIL when it returns."
  (handler-case
      (progn (funcall thunk) nil)
    (sexp-config-error (condition)
      (sexp-config-error-kind condition))))
