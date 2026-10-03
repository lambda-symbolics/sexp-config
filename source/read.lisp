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

(defun read--octet-vector (stream)
  "Read the rest of one #( ) vector whose opening was already read, as octets."
  (let ((elements (read-delimited-list #\) stream t)))
    (unless (every (lambda (element) (typep element '(integer 0 255))) elements)
      (read--reject-syntax :invalid-value
                           "A #( ) vector may hold only integers from 0 to 255."))
    (coerce elements '(vector (unsigned-byte 8)))))

(defun read--dispatch-function (grammar)
  "Return the # reader for GRAMMAR, admitting only the syntax it permits.

A nested block comment returns no values, so it reads as whitespace the way the
standard reader treats it; an octet vector returns its vector."
  (lambda (stream character)
    (declare (ignore character))
    (let ((next (read-char stream nil nil)))
      (cond
        ((and (eql next #\|) (source-grammar-block-comments-permitted-p grammar))
         (read--skip-block-comment stream))
        ((and (eql next #\() (source-grammar-octet-vectors-permitted-p grammar))
         (read--octet-vector stream))
        (t
         (read--reject-syntax
          :invalid-syntax
          (format nil "Unsupported reader syntax #~@[~C~]." next)))))))

(defun read--restricted-readtable (grammar)
  "Return a fresh standard readtable narrowed to what GRAMMAR accepts.

Quoting and dispatch are removed. The scan refuses those characters first;
narrowing the readtable as well keeps a change on either side from widening the
accepted grammar on its own. The dispatch character survives only as the opening
of a block comment or an octet vector, and only when GRAMMAR permits one."
  (let ((readtable (copy-readtable nil)))
    (dolist (character '(#\' #\` #\,))
      (set-macro-character character #'read--unsupported-syntax nil readtable))
    (if (or (source-grammar-block-comments-permitted-p grammar)
            (source-grammar-octet-vectors-permitted-p grammar))
        (set-macro-character #\# (read--dispatch-function grammar) t readtable)
        (set-macro-character #\# #'read--unsupported-syntax t readtable))
    readtable))

(defun read--form (stream grammar locate)
  "Read and validate the one form STREAM holds, after its scan succeeded.

LOCATE maps a stream position to its one-based line for diagnostics. The form is
read with evaluation disabled inside a throwaway package that is deleted
afterward."
  (let ((position 0)
        (start (or (file-position stream) 0))
        (reader-package
          (make-package (symbol-name (gensym "SEXP-CONFIG-READER-"))
                        :use (when (source-grammar-common-lisp-symbols-permitted-p
                                    grammar)
                               '(#:cl)))))
    (flet ((fail-at (kind offset control &rest arguments)
             (error 'sexp-config-error
                    :kind kind
                    :label (source-grammar-label grammar)
                    :offset offset
                    :line (funcall locate offset)
                    :message (apply #'format nil control arguments))))
      (unwind-protect
           (handler-case
               (let ((*package* reader-package)
                     (*read-eval* nil)
                     (*read-suppress* nil)
                     (*read-base* 10)
                     (*read-default-float-format*
                       (source-grammar-read-default-float-format grammar))
                     (*readtable* (read--restricted-readtable grammar))
                     (end (list :end)))
                 ;; This handler declines, so the enclosing HANDLER-CASE still
                 ;; runs. It exists only to record how far the reader had got
                 ;; while the stream is still open.
                 (handler-bind
                     ((serious-condition
                        (lambda (condition)
                          (declare (ignore condition))
                          (setf position
                                (- (or (ignore-errors (file-position stream)) start)
                                   start)))))
                   (let ((form (read stream nil end)))
                     (when (eq form end)
                       (fail-at :no-form 0 "~A contains no form."
                                (source-grammar-label grammar)))
                     (unless (eq (read stream nil end) end)
                       (fail-at :multiple-forms
                                (- (or (file-position stream) start) start)
                                "~A must contain exactly one top-level form."
                                (source-grammar-label grammar)))
                     (validate-tree form grammar))))
             (sexp-config-error (condition)
               (error condition))
             (serious-condition (cause)
               (fail-at :read-error position "Could not read ~A: ~A"
                        (source-grammar-label grammar) cause)))
        (delete-package reader-package)))))

(defun read-source (source grammar)
  "Read and return the one form SOURCE holds, as bounded by GRAMMAR.

SOURCE is scanned before it reaches the reader, read with evaluation disabled
inside a throwaway package that is deleted afterward, then structurally
validated. Every rejection signals SEXP-CONFIG-ERROR, so a host with its own
diagnostics translates that condition at this boundary.

The caller supplies SOURCE, so bounding how much text is read from a file, and
confining which files may be read at all, remain the caller's responsibility;
READ-SOURCE-FILE does both for one file."
  (scan-source source grammar)
  (with-input-from-string (stream source)
    (read--form stream grammar
                (lambda (offset) (grammar--source-line source offset)))))

(defun read-source-file (pathname grammar &key maximum-octets (external-format :utf-8))
  "Read and return the one form the file at PATHNAME holds, as bounded by GRAMMAR.

The file is scanned and read straight from disk, so a large source never becomes
one string. A file longer than MAXIMUM-OCTETS, when given, is refused before
anything is read. Rejections carry octet offsets into the file."
  (let ((label (source-grammar-label grammar)))
    (handler-case
        (progn
          (when maximum-octets
            (with-open-file (stream pathname :element-type '(unsigned-byte 8))
              (when (> (file-length stream) maximum-octets)
                (error 'sexp-config-error
                       :kind :data-too-large
                       :label label
                       :offset 0
                       :line 1
                       :message (format nil "~A exceeds the size limit of ~:D octets."
                                        label maximum-octets)))))
          (with-open-file (stream pathname :external-format external-format)
            (scan--stream stream grammar :octet-offsets-p t)
            (file-position stream 0)
            (read--form stream grammar
                        (lambda (offset) (read--file-line pathname offset)))))
      (sexp-config-error (condition)
        (error condition))
      (file-error (cause)
        (error 'sexp-config-error
               :kind :read-error
               :label label
               :message (format nil "Could not read ~A: ~A" label cause))))))

(defun read--file-line (pathname offset)
  "Return the one-based line of the file at PATHNAME containing octet OFFSET."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (1+ (loop for index below offset
              for octet = (read-byte stream nil nil)
              while octet
              count (= octet 10)))))
