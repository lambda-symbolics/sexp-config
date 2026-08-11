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

(defun scan--validate-keyword-token (source start grammar)
  "Validate the keyword token beginning at START in SOURCE."
  (let* ((end (or (position-if #'scan--delimiter-p source :start (1+ start))
                  (length source)))
         (token (subseq source start end)))
    (when (find-if (lambda (character)
                     (find character '(#\: #\\ #\| #\#)))
                   token
                   :start 1)
      (grammar--fail-token
       grammar :invalid-syntax source start token
       "~A does not permit escaped or package-qualified symbols."
       (source-grammar-label grammar)))
    (unless (scan--accepted-keyword-p token grammar)
      (grammar--fail-token
       grammar :unknown-field source start token
       "~A contains unknown keyword token ~A."
       (source-grammar-label grammar)
       token))
    nil))

(defun scan-source (source grammar)
  "Reject reader syntax and nesting outside GRAMMAR before SOURCE is read.

The scan runs before the reader so that unsupported syntax is refused by
inspection rather than by trusting reader settings. It rejects dispatch, quote,
backquote, comma, escape, and multiple-escape characters, package-qualified and
escaped symbols, keyword tokens outside the grammar, unbalanced or overly deep
parentheses, an unterminated string, and an unterminated block comment.

Line comments and string contents are skipped, so a quoted body may contain any
of those characters. When the grammar permits them, nested block comments are
skipped the same way. Every rejection carries the offset and line where it was
found."
  (let ((length (length source))
        (depth 0)
        (block-comment-depth 0)
        (in-string-p nil)
        (escaped-p nil)
        (in-line-comment-p nil)
        (block-comments-p (source-grammar-block-comments-permitted-p grammar))
        (label (source-grammar-label grammar)))
    (loop with index = 0
          while (< index length)
          do (let ((character (char source index))
                   (next (and (< (1+ index) length) (char source (1+ index))))
                   (step 1))
               (cond
                 (in-line-comment-p
                  (when (find character '(#\Newline #\Return))
                    (setf in-line-comment-p nil)))
                 (in-string-p
                  (cond
                    (escaped-p
                     (setf escaped-p nil))
                    ((char= character #\\)
                     (setf escaped-p t))
                    ((char= character #\")
                     (setf in-string-p nil))))
                 ((plusp block-comment-depth)
                  (cond
                    ((and (char= character #\#) (eql next #\|))
                     (incf block-comment-depth)
                     (setf step 2))
                    ((and (char= character #\|) (eql next #\#))
                     (decf block-comment-depth)
                     (setf step 2))))
                 ((char= character #\;)
                  (setf in-line-comment-p t))
                 ((char= character #\")
                  (setf in-string-p t))
                 ((and block-comments-p
                       (char= character #\#)
                       (eql next #\|))
                  (setf block-comment-depth 1
                        step 2))
                 ((find character '(#\# #\' #\` #\, #\\ #\|))
                  (grammar--fail-at grammar :invalid-syntax source index
                                    "~A uses unsupported reader syntax ~S."
                                    label character))
                 ((char= character #\:)
                  (when (and (plusp index)
                             (not (scan--boundary-p (char source (1- index)))))
                    (grammar--fail-at
                     grammar :invalid-syntax source index
                     "~A does not permit package-qualified symbols."
                     label))
                  (scan--validate-keyword-token source index grammar))
                 ((char= character #\()
                  (incf depth)
                  (when (> depth (source-grammar-maximum-depth grammar))
                    (grammar--fail-at
                     grammar :data-too-deep source index
                     "~A exceeds the structural depth limit of ~D."
                     label
                     (source-grammar-maximum-depth grammar))))
                 ((char= character #\))
                  (decf depth)
                  (when (minusp depth)
                    (grammar--fail-at
                     grammar :invalid-syntax source index
                     "~A contains an unmatched closing parenthesis."
                     label))))
               (incf index step)))
    (when (plusp block-comment-depth)
      (grammar--fail-at grammar :invalid-syntax source length
                        "~A contains an unterminated block comment."
                        label))
    (when in-string-p
      (grammar--fail-at grammar :invalid-syntax source length
                        "~A contains an unterminated string."
                        label))
    (unless (zerop depth)
      (grammar--fail-at grammar :invalid-syntax source length
                        "~A contains unbalanced parentheses."
                        label))
    nil))
