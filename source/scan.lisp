(in-package #:sexp-config)

;;;; -- Source Scanning --

(defun scan--delimiter-p (character)
  "Return true when CHARACTER ends a token."
  (not (null (find character
                   '(#\( #\) #\; #\Space #\Tab #\Newline #\Return #\Page)))))

(defun scan--accepted-keyword-p (token grammar)
  "Return true when TOKEN names a keyword GRAMMAR accepts."
  (let ((keywords (source-grammar-keywords grammar)))
    (and (plusp (length token))
         (char= (char token 0) #\:)
         (or (null keywords)
             (not (null
                   (some (lambda (keyword)
                           (string-equal token
                                         (format nil ":~A" (symbol-name keyword))))
                         keywords)))))))

(defun scan--validate-keyword-token (source start grammar)
  "Validate the keyword token beginning at START in SOURCE."
  (let* ((end (or (position-if #'scan--delimiter-p source :start start)
                  (length source)))
         (token (subseq source start end)))
    (when (find-if (lambda (character)
                     (find character '(#\: #\\ #\| #\#)))
                   token
                   :start 1)
      (grammar--fail-token
       grammar :invalid-syntax token
       "~A does not permit escaped or package-qualified symbols."
       (source-grammar-label grammar)))
    (unless (scan--accepted-keyword-p token grammar)
      (grammar--fail-token
       grammar :unknown-field token
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
parentheses, and an unterminated string. Comments and string contents are
skipped so that a quoted body may contain any of those characters."
  (let ((depth 0)
        (in-string-p nil)
        (escaped-p nil)
        (in-comment-p nil)
        (label (source-grammar-label grammar)))
    (loop for character across source
          for index from 0
          do (cond
               (in-comment-p
                (when (char= character #\Newline)
                  (setf in-comment-p nil)))
               (in-string-p
                (cond
                  (escaped-p
                   (setf escaped-p nil))
                  ((char= character #\\)
                   (setf escaped-p t))
                  ((char= character #\")
                   (setf in-string-p nil))))
               ((char= character #\;)
                (setf in-comment-p t))
               ((char= character #\")
                (setf in-string-p t))
               ((find character '(#\# #\' #\` #\, #\\ #\|))
                (grammar--fail grammar :invalid-syntax
                               "~A uses unsupported reader syntax ~S."
                               label character))
               ((char= character #\:)
                (when (and (plusp index)
                           (not (scan--delimiter-p (char source (1- index)))))
                  (grammar--fail grammar :invalid-syntax
                                 "~A does not permit package-qualified symbols."
                                 label))
                (scan--validate-keyword-token source index grammar))
               ((char= character #\()
                (incf depth)
                (when (> depth (source-grammar-maximum-depth grammar))
                  (grammar--fail grammar :data-too-deep
                                 "~A exceeds the structural depth limit of ~D."
                                 label
                                 (source-grammar-maximum-depth grammar))))
               ((char= character #\))
                (decf depth)
                (when (minusp depth)
                  (grammar--fail grammar :invalid-syntax
                                 "~A contains an unmatched closing parenthesis."
                                 label)))))
    (when in-string-p
      (grammar--fail grammar :invalid-syntax
                     "~A contains an unterminated string."
                     label))
    (unless (zerop depth)
      (grammar--fail grammar :invalid-syntax
                     "~A contains unbalanced parentheses."
                     label))
    nil))
