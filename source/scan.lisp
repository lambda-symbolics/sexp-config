(in-package #:sexp-config)

;;;; -- Source Scanning --

(defun scan--delimiter-p (character)
  "Return true when CHARACTER ends a token.

This is deliberately the standard set and not every character the scan refuses.
A refused character stays inside the token it appears in, so the token check can
report it as escaped or package-qualified syntax rather than as an unknown
keyword."
  (not (null (find character
                   '(#\( #\) #\; #\" #\Space #\Tab #\Newline #\Return #\Page)))))

(defun scan--boundary-p (character)
  "Return true when CHARACTER may precede a keyword token.

A keyword may follow a delimiter, a closing quote, or a closed block comment, so
this set is wider than SCAN--DELIMITER-P. Anything else before a colon means the
colon is qualifying a name."
  (not (null (or (scan--delimiter-p character)
                 (find character '(#\" #\# #\' #\` #\, #\\ #\|))))))

(defun scan--accepted-keyword-p (token grammar)
  "Return true when TOKEN names a keyword GRAMMAR accepts.

TOKEN still carries its leading colon; a grammar predicate receives the name
without it."
  (let ((keywords (source-grammar-keywords grammar))
        (predicate (source-grammar-keyword-predicate grammar)))
    (and (plusp (length token))
         (char= (char token 0) #\:)
         (or (and (null keywords) (null predicate))
             (not (null
                   (some (lambda (keyword)
                           (string-equal token
                                         (format nil ":~A" (symbol-name keyword))))
                         keywords)))
             (and predicate
                  (not (null (funcall predicate (subseq token 1)))))))))

(defun scan--escaped-token-p (token)
  "Return true when TOKEN, after its first character, holds escape or qualifier syntax."
  (not (null (find-if (lambda (character)
                        (find character '(#\: #\\ #\| #\#)))
                      token
                      :start 1))))

(defun scan--utf-8-length (character)
  "Return how many octets CHARACTER occupies in UTF-8."
  (let ((code (char-code character)))
    (cond
      ((< code #x80) 1)
      ((< code #x800) 2)
      ((< code #x10000) 3)
      (t 4))))

(defun scan-source (source grammar)
  "Reject reader syntax and nesting outside GRAMMAR before SOURCE is read.

The scan runs before the reader so that unsupported syntax is refused by
inspection rather than by trusting reader settings. It rejects dispatch, quote,
backquote, comma, escape, and multiple-escape characters, package-qualified and
escaped symbols, keyword tokens outside the grammar, unbalanced or overly deep
parentheses, an unterminated string, and an unterminated block comment.

Line comments and string contents are skipped, so a quoted body may contain any
of those characters. When the grammar permits them, nested block comments are
skipped the same way, octet vectors open like lists, and COMMON-LISP qualified
symbols pass. Every rejection carries the offset and line where it was found."
  (with-input-from-string (stream source)
    (scan--stream stream grammar)))

(defun scan--stream (stream grammar &key octet-offsets-p)
  "Scan every character of STREAM as SCAN-SOURCE scans a string.

Offsets count characters, or UTF-8 octets when OCTET-OFFSETS-P."
  (let ((offset 0)
        (line 1)
        (previous nil)
        (depth 0)
        (nodes 0)
        (in-token-p nil)
        (string-characters 0)
        (array-depth nil)
        (array-nodes 0)
        (string-bound (source-grammar-maximum-string-characters grammar))
        (block-comment-depth 0)
        (in-string-p nil)
        (escaped-p nil)
        (in-line-comment-p nil)
        (bare (make-string-output-stream))
        (block-comments-p (source-grammar-block-comments-permitted-p grammar))
        (octet-vectors-p (source-grammar-octet-vectors-permitted-p grammar))
        (readable-strings-p (source-grammar-readable-strings-permitted-p grammar))
        (qualified-p (source-grammar-qualified-common-lisp-symbols-permitted-p grammar))
        (label (source-grammar-label grammar)))
    (labels ((fail-at (kind at-offset at-line token control &rest arguments)
               (error 'sexp-config-error
                      :kind kind :label label :offset at-offset :line at-line
                      :token token
                      :message (apply #'format nil control arguments)))

             (fail (kind control &rest arguments)
               (apply #'fail-at kind offset line nil control arguments))

             (advance (character)
               (incf offset (if octet-offsets-p (scan--utf-8-length character) 1))
               (when (char= character #\Newline)
                 (incf line))
               (setf previous character))

             (consume ()
               (let ((character (read-char stream nil nil)))
                 (when character
                   (advance character))
                 character))

             (peek ()
               (peek-char nil stream nil nil))

             (token-rest (prefix)
               (with-output-to-string (token)
                 (write-string prefix token)
                 (loop for next = (peek)
                       while (and next (not (scan--delimiter-p next)))
                       do (write-char (consume) token))))

             (count-node (at-offset at-line)
               (if array-depth
                   (when (> (incf array-nodes) 16)
                     (fail-at :invalid-syntax at-offset at-line nil
                              "~A contains an oversized #A string descriptor." label))
                   (when (> (incf nodes) (source-grammar-maximum-nodes grammar))
                     (fail-at :data-too-large at-offset at-line nil
                              "~A exceeds the source node limit of ~D."
                              label (source-grammar-maximum-nodes grammar)))))

             (open-list (at-offset at-line)
               (count-node at-offset at-line)
               (incf depth)
               (when (> depth (source-grammar-maximum-depth grammar))
                 (fail-at :data-too-deep at-offset at-line nil
                          "~A exceeds the structural depth limit of ~D."
                          label (source-grammar-maximum-depth grammar))))

             (colon (token-offset token-line before)
               (if (and before (not (scan--boundary-p before)))
                   (let ((package-name (get-output-stream-string bare)))
                     (unless (and qualified-p
                                  (member package-name '("CL" "COMMON-LISP")
                                          :test #'string-equal)
                                  (not (eql (peek) #\:))
                                  (not (scan--escaped-token-p (token-rest ":"))))
                       (fail-at :invalid-syntax token-offset token-line nil
                                "~A does not permit package-qualified symbols."
                                label)))
                   (let ((token (token-rest ":")))
                     (when (scan--escaped-token-p token)
                       (fail-at :invalid-syntax token-offset token-line token
                                "~A does not permit escaped or package-qualified symbols."
                                label))
                     (unless (scan--accepted-keyword-p token grammar)
                       (fail-at :unknown-field token-offset token-line token
                                "~A contains unknown keyword token ~A."
                                label token))))))
      (loop for character = (read-char stream nil nil)
            while character
            do (let ((token-offset offset)
                     (token-line line)
                     (before previous))
                 (advance character)
                 (cond
                   (in-line-comment-p
                    (when (find character '(#\Newline #\Return))
                      (setf in-line-comment-p nil)))
                   (in-string-p
                    (when (or escaped-p (not (find character '(#\" #\\))))
                      (when (and string-bound (> (incf string-characters) string-bound))
                        (fail-at :data-too-large token-offset token-line nil
                                 "~A contains a string longer than ~D characters."
                                 label string-bound)))
                    (cond
                      (escaped-p
                       (setf escaped-p nil))
                      ((char= character #\\)
                       (setf escaped-p t))
                      ((char= character #\")
                       (setf in-string-p nil))))
                   ((plusp block-comment-depth)
                    (cond
                      ((and (char= character #\#) (eql (peek) #\|))
                       (consume)
                       (incf block-comment-depth))
                      ((and (char= character #\|) (eql (peek) #\#))
                       (consume)
                       (decf block-comment-depth))))
                   ((char= character #\;)
                    (setf in-line-comment-p t))
                   ((char= character #\")
                    (count-node token-offset token-line)
                    (setf in-string-p t string-characters 0))
                   ((and block-comments-p (char= character #\#) (eql (peek) #\|))
                    (consume)
                    (setf block-comment-depth 1))
                   ((and octet-vectors-p (char= character #\#) (eql (peek) #\())
                    (open-list token-offset token-line)
                    (consume))
                   ((and readable-strings-p (char= character #\#)
                         (member (peek) '(#\a #\A)))
                    (when array-depth
                      (fail-at :invalid-syntax token-offset token-line nil
                               "~A contains a nested #A descriptor." label))
                    (count-node token-offset token-line)
                    (setf array-depth (1+ depth) array-nodes 0)
                    (consume))
                   ((find character '(#\# #\' #\` #\, #\\ #\|))
                    (fail-at :invalid-syntax token-offset token-line nil
                             "~A uses unsupported reader syntax ~S."
                             label character))
                   ((char= character #\:)
                    (unless (and before (not (scan--boundary-p before)))
                      (count-node token-offset token-line))
                    (colon token-offset token-line before))
                   ((char= character #\()
                    (open-list token-offset token-line))
                   ((char= character #\))
                    (when (eql depth array-depth)
                      (setf array-depth nil))
                    (decf depth)
                    (when (minusp depth)
                      (fail-at :invalid-syntax token-offset token-line nil
                               "~A contains an unmatched closing parenthesis."
                               label)))
                   ((and (not in-token-p) (not (scan--delimiter-p character)))
                    (unless (char= character #\.)
                      (count-node token-offset token-line))))
                 (setf in-token-p
                       (and (not in-string-p) (not in-line-comment-p)
                            (zerop block-comment-depth)
                            (not (scan--delimiter-p character))))
                 (if (or in-string-p in-line-comment-p (plusp block-comment-depth)
                         (scan--boundary-p character))
                     (get-output-stream-string bare)
                     (write-char character bare))))
      (when (plusp block-comment-depth)
        (fail :invalid-syntax "~A contains an unterminated block comment." label))
      (when in-string-p
        (fail :invalid-syntax "~A contains an unterminated string." label))
      (unless (zerop depth)
        (fail :invalid-syntax "~A contains unbalanced parentheses." label))
      nil)))
