(in-package #:sexp-config)

;;;; -- Structure Validation --

(defun validate-tree (form grammar)
  "Return FORM after rejecting structure GRAMMAR does not accept.

Circular and shared list structure is always rejected, because a caller that
walks the result must terminate. Depth, node count, string length, shared
strings, dotted lists, and permitted atoms follow GRAMMAR.

Structure is validated after reading, so these rejections carry no offset."
  (let ((seen (make-hash-table :test #'eq))
        (nodes 0)
        (label (source-grammar-label grammar))
        (predicate (source-grammar-allowed-atom-predicate grammar))
        (tails-deepen-p (source-grammar-list-tails-increase-depth-p grammar))
        (improper-p (source-grammar-improper-lists-permitted-p grammar))
        (shared-strings-p (source-grammar-shared-strings-permitted-p grammar))
        (string-bound (source-grammar-maximum-string-characters grammar)))
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
              (when (stringp value)
                (unless shared-strings-p
                  (when (gethash value seen)
                    (grammar--fail grammar :invalid-structure
                                   "~A contains a shared string."
                                   label))
                  (setf (gethash value seen) t))
                (when (and string-bound (> (length value) string-bound))
                  (grammar--fail grammar :data-too-large
                                 "~A contains a string longer than ~D characters."
                                 label string-bound)))
              (when (and predicate (not (funcall predicate value)))
                (grammar--fail grammar :invalid-value
                               "~A contains an unsupported value."
                               label))))))
      (walk form 0 nil))
    form))


;;;; -- Restricted Reading --

(defun read--reject-syntax (kind message)
  "Signal a reader-level rejection that the scan should already have made.

These carry no label or position: the scan runs first and reports both, so
reaching one of these means the scan and the readtable disagree."
  (error 'sexp-config-error
         :kind kind
         :label "the configuration source"
         :message message))

(defun read--unsupported-syntax (stream character)
  "Reject CHARACTER, which the scan should already have refused."
  (declare (ignore stream))
  (read--reject-syntax
   :invalid-syntax
   (format nil "Unsupported reader syntax ~S." character)))

(defun read--skip-block-comment (stream)
  "Consume one nested block comment whose opening #| was already read."
  (let ((depth 1))
    (loop
      (when (zerop depth)
        (return))
      (let ((character (read-char stream nil nil)))
        (cond
          ((null character)
           (read--reject-syntax :invalid-syntax
                                "Unterminated block comment."))
          ((and (char= character #\#)
                (eql (peek-char nil stream nil nil) #\|))
           (read-char stream nil nil)
           (incf depth))
          ((and (char= character #\|)
                (eql (peek-char nil stream nil nil) #\#))
           (read-char stream nil nil)
           (decf depth))))))
  (values))

(defun read--dispatch-syntax (stream character)
  "Read a nested block comment, and reject every other use of CHARACTER.

Returning no values makes a block comment behave as whitespace, which is how
the standard reader treats it."
  (declare (ignore character))
  (let ((next (read-char stream nil nil)))
    (unless (eql next #\|)
      (read--reject-syntax
       :invalid-syntax
       (format nil "Unsupported reader syntax #~@[~C~]." next)))
    (read--skip-block-comment stream)))

(defun read--restricted-readtable (grammar)
  "Return a fresh standard readtable narrowed to what GRAMMAR accepts.

Quoting and dispatch are removed. The scan refuses those characters first;
narrowing the readtable as well keeps a change on either side from widening the
accepted grammar on its own. The dispatch character survives only as the opening
of a block comment, and only when GRAMMAR permits one."
  (let ((readtable (copy-readtable nil)))
    (dolist (character '(#\' #\` #\,))
      (set-macro-character character #'read--unsupported-syntax nil readtable))
    (if (source-grammar-block-comments-permitted-p grammar)
        (set-macro-character #\# #'read--dispatch-syntax t readtable)
        (set-macro-character #\# #'read--unsupported-syntax t readtable))
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
  (let ((position 0)
        (reader-package
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
                   (*readtable* (read--restricted-readtable grammar))
                   (end (list :end)))
               (with-input-from-string (stream source)
                 ;; This handler declines, so the enclosing HANDLER-CASE still
                 ;; runs. It exists only to record how far the reader had got
                 ;; while the stream is still open.
                 (handler-bind
                     ((serious-condition
                        (lambda (condition)
                          (declare (ignore condition))
                          (setf position
                                (or (ignore-errors (file-position stream)) 0)))))
                   (let ((form (read stream nil end)))
                     (when (eq form end)
                       (grammar--fail-at grammar :no-form source 0
                                         "~A contains no form."
                                         (source-grammar-label grammar)))
                     (unless (eq (read stream nil end) end)
                       (grammar--fail-at
                        grammar :multiple-forms source
                        (or (file-position stream) 0)
                        "~A must contain exactly one top-level form."
                        (source-grammar-label grammar)))
                     (validate-tree form grammar)))))
           (sexp-config-error (condition)
             (error condition))
           (serious-condition (cause)
             (grammar--fail-at grammar :read-error source position
                               "Could not read ~A: ~A"
                               (source-grammar-label grammar) cause)))
      (delete-package reader-package))))
