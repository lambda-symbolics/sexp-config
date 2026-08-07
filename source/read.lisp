(in-package #:sexp-config)

;;;; -- Structure Validation --

(defun validate-tree (form grammar)
  "Return FORM after rejecting structure GRAMMAR does not accept.

Circular and shared list structure is always rejected, because a caller that
walks the result must terminate. Depth, node count, dotted lists, and permitted
atoms follow GRAMMAR."
  (let ((seen (make-hash-table :test #'eq))
        (nodes 0)
        (label (source-grammar-label grammar))
        (predicate (source-grammar-allowed-atom-predicate grammar))
        (tails-deepen-p (source-grammar-list-tails-increase-depth-p grammar))
        (improper-p (source-grammar-improper-lists-permitted-p grammar)))
    (labels
        ((walk (value depth list-tail-p)
           (when (> depth (source-grammar-maximum-depth grammar))
             (grammar--fail grammar :data-too-deep
                            "~A exceeds the structural depth limit of ~D."
                            label (source-grammar-maximum-depth grammar)))
           (incf nodes)
           (when (> nodes (source-grammar-maximum-nodes grammar))
             (grammar--fail grammar :data-too-large
                            "~A exceeds the structural node limit of ~D."
                            label (source-grammar-maximum-nodes grammar)))
           (cond
             ((consp value)
              (when (gethash value seen)
                (grammar--fail grammar :invalid-structure
                               "~A contains circular or shared list structure."
                               label))
              (setf (gethash value seen) t)
              (walk (first value) (1+ depth) nil)
              (walk (rest value) (if tails-deepen-p (1+ depth) depth) t))
             (t
              (when (and list-tail-p value (not improper-p))
                (grammar--fail grammar :invalid-structure
                               "~A contains an improper list."
                               label))
              (when (and predicate (not (funcall predicate value)))
                (grammar--fail grammar :invalid-value
                               "~A contains an unsupported value."
                               label))))))
      (walk form 0 nil))
    form))


;;;; -- Restricted Reading --

(defun read--unsupported-syntax (stream character)
  "Reject CHARACTER, which the scan should already have refused."
  (declare (ignore stream))
  (error 'sexp-config-error
         :kind :invalid-syntax
         :label "the configuration source"
         :message (format nil "Unsupported reader syntax ~S." character)))

(defun read--restricted-readtable ()
  "Return a fresh standard readtable with dispatch and quoting disabled.

The scan refuses these characters first. Disabling them again keeps a reader
change from widening the accepted grammar by accident."
  (let ((readtable (copy-readtable nil)))
    (dolist (character '(#\# #\' #\` #\,))
      (set-macro-character character #'read--unsupported-syntax nil readtable))
    readtable))

(defun read-source (source grammar)
  "Read and return the one form SOURCE holds, as bounded by GRAMMAR.

SOURCE is scanned before it reaches the reader, read with evaluation disabled
inside a throwaway package that is deleted afterward, then structurally
validated. Every rejection signals SEXP-CONFIG-ERROR, so a host with its own
diagnostics translates that condition at this boundary.

The caller supplies SOURCE, so bounding how much text is read from a file, and
confining which files may be read at all, remain the caller's responsibility."
  (scan-source source grammar)
  (let ((reader-package
          (make-package (symbol-name (gensym "SEXP-CONFIG-READER-"))
                        :use (when (source-grammar-common-lisp-symbols-permitted-p
                                    grammar)
                               '(#:cl)))))
    (unwind-protect
         (handler-case
             (let ((*package* reader-package)
                   (*read-eval* nil)
                   (*read-suppress* nil)
                   (*read-base* 10)
                   (*read-default-float-format* 'double-float)
                   (*readtable* (read--restricted-readtable))
                   (end (list :end)))
               (with-input-from-string (stream source)
                 (let ((form (read stream nil end)))
                   (when (eq form end)
                     (grammar--fail grammar :no-form
                                    "~A contains no form."
                                    (source-grammar-label grammar)))
                   (unless (eq (read stream nil end) end)
                     (grammar--fail grammar :multiple-forms
                                    "~A must contain exactly one top-level form."
                                    (source-grammar-label grammar)))
                   (validate-tree form grammar))))
           (sexp-config-error (condition)
             (error condition))
           (serious-condition (cause)
             (grammar--fail grammar :read-error
                            "Could not read ~A: ~A"
                            (source-grammar-label grammar) cause)))
      (delete-package reader-package))))
