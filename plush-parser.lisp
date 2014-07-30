(in-package :plush-parser)

(declaim (optimize (debug 3)))

(define-condition eof-when-tokenizing (simple-error) ())

#+(or)
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (asdf:load-system :printv))
  (defmacro printv (arg)
    `(printv:printv ,arg)))
#-(or)
(defmacro printv (arg)
  arg)


(defparameter *newline-skip-list* (list (cons 0 0)))

(defun update-newline-skip (position value)
  (setf *newline-skip-list*
	(acons position value
	       (delete-if (lambda (x) (>= x position))
			  *newline-skip-list* :key #'car))))

(defun get-newline-skip-for-position (pos)
  (loop for (position . value)
       in *newline-skip-list*
       when (<= position pos) return value))




(defrule eof-token 
         (! character)
         (:constant (list :eof)))

(defun one-lisp-form (input start end)
  (let* ((end
           (handler-case
             (nth-value 1 (read-from-string input t nil :start start :end end))
             (error () (error
		       (make-condition 'eof-when-tokenizing :format-control "EOF in lisp-form"))))))
    (values (subseq input start end) end t)))

(defun not-newline (char)
  (char/= #\Newline char))

;TODO esrap set newline-skip
(defun here-doc-internal (input start end)
  ;(format t "HDI: ~a ~a ~a~%" input start end)
  (multiple-value-bind (word word-end info)
    (esrap:parse 'posix-word input :start start :end end :junk-allowed t)
      (declare (ignorable info))
    ;(format t "HDI: ~a ~a ~a~%" word word-end info)
      (if (or (null word-end)
	      (= word-end start))
	  (values nil start "Error parsing here-doc")
	  (multiple-value-bind
		(doc newend success)
	      (esrap:parse
	       `(and
		 (* (and (! #\Newline) character))
		 #\Newline
		 (string ,(get-newline-skip-for-position start))
		 (*
		  (and
		   (! ,(concatenate 'string
				    '(#\Newline)
				    (second word)))
		   character))
		 ,(concatenate 'string
			       '(#\Newline)
			       (second word)))
	       input
	       :start word-end :end end :junk-allowed t)
	    (declare (ignore newend) (type))
					;(format t "HDI: ~a ~a ~a~%" doc newend success)
	    (if (not (eql t success))
		(values nil start "Error parsing here-doc")
		(let ((text (text (nthcdr 3 doc))))
		  (update-newline-skip (1+ start) (+ (get-newline-skip-for-position start) (length text)))
		  (values (list :here-doc (subseq text 0 (- (length text) (1+ (length word)))) (length text)) end t))))
	  )))

    
;This has a side-effect and so relies on not-backtracking
;Since operators are unambiguous that should be fine

(defrule here-doc-reader
   (&
    (function here-doc-internal)))

(defrule here-doc-op
     (and
       (or "<<-" "<<")
       here-doc-reader)
     (:destructure (op doc)
                   (list (make-keyword op) op doc)))


(defrule operator-start
   (or "|" "&" ";" "<" ">" "(" ")"))


(defrule operator-token
  (or here-doc-op
      "&&" "||" ";;" ">|" ">>" "<&"
      ">&" "<>" "|" "&" ";"
      "<" ">" "(" ")")
  (:lambda (x)
    (if (stringp x) (list (make-keyword x) x)
      x)))

(defrule line-comment
    (and
      "#"
      (*
        (and
          (! #\Newline)
          character)))
    (:constant nil))

(defun newline-skip (input start end)
  (declare (ignorable input))
  (printv (get-newline-skip-for-position start))
  (let ((skip (get-newline-skip-for-position start)))
    (update-newline-skip (+ start skip) 0)
    (if
     (>= end (+ start skip))
     (values nil (+ start skip) t)
     (values nil start))))

(defrule newline-token
         (and #\Newline
              (function newline-skip))
         (:constant (list :newline #.(string #\Newline))))

(defrule io-number
    (and
      (* (digit-char-p character))
      (& (or "<" ">")))
    (:destructure (num _)
		  (declare (ignore _))
                  (list :io-number (text num))))

(defrule blank-character
  (or #\Space #\Page #\Return #\Tab #\Vt
      #.(coerce '(#\\ #\Newline) 'string))
  (:constant nil))

(defrule backslash-quote
    (and #\\ character)
    (:text t))

(defun not-dquote (x)
  (char/= #\" x))

(defun not-squote (x)
  (char/= #\' x))

(defun dquote-error (&rest x)
  (declare (ignore x))
  (error (make-condition 'eof-when-tokenizing :format-control "EOF looking for \"")))

(defun squote-error (&rest x)
  (declare (ignore x))
  (error (make-condition 'eof-when-tokenizing :format-control "EOF looking for \'"))) 

(defrule double-quoted-meat
  (and
    #\"
    (or (and
          (*
	   (and
	    (! #\")
	    (or
	     backslash-quote
	     arithmetic-expansion
	     command-substitution
	     parameter-expansion
	     lisp-expansion
	     (not-dquote character))))
          #\")
        (function dquote-error)))
    (:text t))

(defrule single-quoted-meat
         (and
           #\'
           (or (and
                 (* (not-squote character))
                 #\')
               (function squote-error)))
         (:text t))

(defrule parenthesized-expression
  (and
    "("
    (* (or "\\)"
           (and (! ")") character)))
    ")")
  (:text t))

(defrule lisp-expansion
  (and
    "$["
    (function one-lisp-form)
    "]")
  (:text t))

(defrule arithmetic-expansion
  (and
    "$(("
    (*
      (and
        (! "))")
        token-string-until-unbalanced-paren))
    "))")
  (:text t))

(defrule command-substitution
  (or
    (and "`"
         (*
	  (or "\$" "\`" "\\\\"
	      (and (! "\`") character)))
         "`")
    (and
      "$("
      token-string-until-unbalanced-paren
      ")"))
  (:text t))

(defmacro token-or-pair (name start end)
  `(progn
     (defun ,(intern (concatenate 'string "EOF-" (symbol-name name)))
       (&rest r)
       (declare (ignore r))
       (error (make-condition 'eof-when-tokenizing :format-control "EOF looking for ~A" :format-arguments (list ,end))))
     (defrule ,name
      (and
        (*
          (or
            (and (! character)
                 (function ,(intern (concatenate 'string "EOF-" (symbol-name name)))))
            (and (! ,end)
                 (or 
                   (and ,start
                        ,name
                        ,end)
                   token-meat
                   (and (! ,start) character))))))
      (:text t))))

(token-or-pair token-string-until-unbalanced-brace "{" "}")
(token-or-pair token-string-until-unbalanced-paren "(" ")")

(defun posix-start-char (x)
  (position x "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"))

(defun posix-entity-char (x)
  (position x "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_1234567890"))

(defrule posix-name
 (and
   (posix-start-char character)
   (* (posix-entity-char character)))
  (:text t))


(defrule parameter-expansion
  (and
    "$"
    (or
      (and
        "{"
        token-string-until-unbalanced-brace
        "}")
      posix-name))
  (:text t))

(defrule token-meat
  (and
    (! operator-start)
    (! eof-token)
    (! #\Newline)
    (! blank-character)
    (or
      backslash-quote
      double-quoted-meat
      single-quoted-meat
      lisp-expansion
      arithmetic-expansion
      command-substitution
      parameter-expansion
      character)))

(defrule token-string
  (and
    (* #.(coerce '(#\\ #\Newline) 'string))
    (* blank-character)
    (* #.(coerce '(#\\ #\Newline) 'string))
    posix-token)
  (:destructure (a b c tok)
                (text (list a b c (second tok)))))

(defrule posix-word
  (+ token-meat)
  (:lambda (x) (list :token (text x))))

(defrule token-boundaries
  (or
    operator-token
    io-number
    newline-token
    eof-token
    posix-word)
  (:lambda (x &bounds start end)
    (append x (list start end 0))))

(defrule posix-token
  (and
    (* blank-character)
    (* line-comment)
    token-boundaries)
  (:destructure (x y z)
		(declare (ignore x y))
		z))

(defun get-posix-tokens (input)
  (loop
       for (result newstart success) = (multiple-value-list (esrap:parse 'posix-token input :junk-allowed t)) then
       (multiple-value-list (esrap:parse 'posix-token input :start (or newstart (length input)) :junk-allowed t))
       collect result
       while (not (eql (car result) :eof))))

(defmacro with-temp-fd ((fd-var name-var &optional (template "/tmp/plush") ignore-unlink-errors) &body body)
  `(multiple-value-bind (,fd-var ,name-var) (isys:mkstemp ,template)
     (unwind-protect
	  (progn
	    ,@body)
       (isys:close ,fd-var)
       ,(if ignore-unlink-errors
	    `(ignore-errors
	       (isys:unlink ,name-var))
	    `(isys:unlink ,name-var)))))

(defrule do-arithmetic-expansion
  (and
   "$(("
   (*
    (and
     (! "))")
     token-string-until-unbalanced-paren))
   "))")
  (:destructure (start stuff end)
		(declare (ignore start end))
    (let* ((expanded
	    (esrap:text
	     (do-some-expansions
		 stuff
		 'do-command-expansion
	       'do-lisp-expansion
	       'do-parameter-expansion
	       'do-quote-expansion
	       'do-backslash-expansion)))
	   (parsed (esrap:parse 'arithmetic-expr expanded)))
      (format nil "~D" (run-arithmetic parsed)))))

(defun is-special-variable (x)
  (member (make-keyword x) plush::+special-variables+))

(defrule do-special-parameter-expansion
   (and
    "$"
    (is-special-variable character))
  (:destructure
   (dollar name)
   (declare (ignore dollar))
   (plush::get-parameter (string name) nil)))

(defrule do-special-parameter-expansion-dquote
   (and
    "$"
    (is-special-variable character))
  (:destructure
   (dollar name)
   (declare (ignore dollar))
   (plush::get-parameter (string name) t)))

(defrule do-numeric-parameter-expansion
    (and "$"
	 (digit-char-p character))
  (:destructure
   (dollar number)
   (declare (ignore dollar))
   (plush::get-parameter (string number))))

(defrule do-double-quote-expansion
    double-quoted-meat
  (:lambda (x)
     (do-some-expansions
	 (subseq x 1 (1- (length x)))
       'do-command-expansion
       'do-lisp-expansion
       'do-parameter-expansion
       'do-quote-expansion
       'do-backslash-expansion)))
       
(defrule do-lisp-expansion
    lisp-expansion
  (:lambda (string)
    (let ((string (subseq string 2 (1- (length string)))))
      (format nil "~a" (eval (read-from-string string))))))

(defrule do-command-expansion
      command-substitution
  (:lambda (string)
    (let ((string
	(subseq string
		(if (equal (char string 0) #\`) 1 2)
		(1- (length string)))))
      (with-temp-fd  (fd fname)
	(plush::with-subshell (t)
	  (isys:dup2 fd iolib/os:+stdout+)
	  (mapc #'eval (plush-parser::parse-posix-stuff string))
	  (isys:exit 0))
	(let ((str (coerce
		    (with-open-file (s fname)
		      (loop for c = (read-char s nil :eof)
			 until (eql c :eof)
			 collect c))
		    'string)))
	  (if (and (> (length str) 0) (char-equal (char str (1- (length str))) #\Newline))
              (subseq str 0 (1- (length str))) str))))))

(defrule parameter-default
  (and
   ":-"
   token-string-until-unbalanced-brace)
   (:destructure (_ word)
		 (declare (ignore _))
    (list :default word)))

(defrule parameter-assign
  (and
   ":="
   token-string-until-unbalanced-brace)
    (:destructure (_ word)
		 (declare (ignore _))
		 (list :assign word)))

(defrule parameter-alternative
    (and
     ":+")
  (:destructure (_ word)
		 (declare (ignore _))
		(list :alternative word)))

(defrule parameter-error
  (and
   ":?"
   token-string-until-unbalanced-brace)
  (:destructure (_ word)
		 (declare (ignore _))
    (list :error word)))

(defrule parameter-option
  (or
   parameter-default
   parameter-assign
   parameter-error
   parameter-alternative))

(defrule do-parameter-brace-expansion
    (and
     "${"
     (*
      (and
       (! #\:)
       (! #\})
       character))
     (? parameter-option)
     "}")
  (:destructure
   (start name option end)
   (declare (ignore start end))
   (let ((name (text name)))
     (let
	 ((value (plush::get-parameter name))
	  (word (when option (second option))))
       (cond
	 ((and option
	       (eql (car option) :alternative))
	  (if (= (length value) 0)
	      ""
	      (plush::expand-word-list (list word))))
	 ((> (length value) 0)
	  value)
	 ((null option)
	  (list ""))
	 ((eql (car option) :default)
	  (plush::expand-word-list (list word)))
	 ((eql (car option) :assign)
	  (let ((v (plush::expand-word-list nil)))
	    (setf (iolib/os:environment-variable name) v)
	    v))
	 ((eql (car option) :error)
	  (format *error-output*
		  "~%Attempt to read from undefined paramter ~A~%" name)))))))

(defrule do-quote-expansion
  single-quoted-meat)

(defrule do-backslash-expansion
  (and #\\ character)
  (:text t))

(defrule tilde-expand-assignment
    (and
     "~"
     (*
      (and
       (! "/")
       (! ":")
      character)))
  (:destructure
   (tilde expandme)
   (declare (ignore tilde))
   (let ((expandme (text expandme)))
     (if (string= expandme "")
	 (iolib/os:environment-variable "HOME")
	 (nth-value 5 (isys:getpwnam expandme))))))

(defrule tilde-expand
    (and
     "~"
     (*
      (and
       (! "/")
       character)))
  (:destructure
   (tilde expandme)
   (declare (ignore tilde))
   (let ((expandme (text expandme)))
     (if (string= expandme "")
	 (iolib/os:environment-variable "HOME")
	 (nth-value 5 (isys:getpwnam expandme))))))

(defrule colon-tilde-expand
    (and
     ":"
     tilde-expand-assignment)
  (:text t))

(defun join-strings-preserving-lists (list)
  (loop
     with this-string = nil
     for item in list
     when (stringp item)
     do (push item this-string)
     else when (characterp item)
     do (push (string item) this-string)
     else
     do (push (car item) this-string)
     and collect (apply #'concatenate 'string (nreverse this-string)) into returnme
     and do (setf this-string (copy-list (last (cdr item))))
     and append (butlast (cdr item)) into returnme
     finally (return `(,@returnme ,@
		       (and this-string
			    (list 
			     (apply #'concatenate 'string (nreverse this-string))))))))

(defrule do-name-parameter-expansion
    (and "$"
	 posix-name)
  (:destructure (dollar name)
		(declare (ignore dollar))
    (let ((v (plush::get-parameter (text name))))
      (or v ""))))

(defrule do-parameter-expansion
    (or
     do-parameter-brace-expansion
     do-lisp-expansion
     do-special-parameter-expansion
     do-numeric-parameter-expansion
     do-name-parameter-expansion))

(defrule do-parameter-expansion-dquote
    (or
     do-parameter-brace-expansion
     do-lisp-expansion
     do-special-parameter-expansion-dquote
     do-numeric-parameter-expansion
     do-name-parameter-expansion))

(defun do-some-expansions (expandme &rest parsers)
  (let ((stuff
	 (esrap:parse `(* (or ,@parsers character)) expandme)))
    (join-strings-preserving-lists stuff)))

					;TODO apply field splitting to results of expansion
(defrule expand-here-doc-parser
    (*
     (or
      do-arithmetic-expansion
      do-command-expansion
      do-parameter-expansion
      do-backslash-expansion
      character))
  (:lambda (rest)
    (join-strings-preserving-lists
     rest)))

(defrule expand-word-parser
  (and
   (? tilde-expand)
   (*
    (or
     do-arithmetic-expansion
     do-command-expansion
     do-parameter-expansion
     do-double-quote-expansion
     do-quote-expansion
     do-backslash-expansion
     character) ))
  (:destructure
   (start rest)
   (join-strings-preserving-lists
    (if start
	(cons start rest)
	rest))))

(defrule expand-word-parser-assignment
  (and
   (? tilde-expand-assignment)
   (*
    (or
     do-arithmetic-expansion
     do-command-expansion
     do-parameter-expansion
     do-double-quote-expansion
     do-quote-expansion
     do-backslash-expansion
     colon-tilde-expand
     character) ))
  (:destructure
   (start rest)
   (join-strings-preserving-lists
    (if start
	(cons start rest)
	rest))))

(defrule glob-equiv (and (! character) character)) ;TODO STUB
(defrule glob-class (and (! character) character)) ;TODO STUB
(defrule glob-collating (and (! character) character)) ;TODO STUB

(defrule glob-bracket
    (and
     "["
     (? "!")
     (*
      (or
       glob-collating
       glob-equiv
       glob-class
       glob-quote
       (and
	(! "]")
	character)))
     "]")
  (:destructure
   (start invert stuff end)
   (declare (ignore start end))
   (if stuff
       `(,(if invert
	     :inverted-char-class
	     :char-class) ,@(alexandria:flatten stuff))
       "[]")))
     

(defrule glob-star
    "*"
  (:constant
   '(:greedy-repetition 0 nil :everything)))

(defrule glob-question
    "?"
  (:constant :everything))

(defrule glob-special
    (or
     glob-star
     glob-question
     glob-bracket))

(defrule glob-quote-as-sequence
    glob-quote
  (:lambda (stuff)
    `(:sequence ,@(or stuff '(:void)))))

(defrule glob-single-quote
    (and #\\
	 character)
  (:lambda (x) (second x)))

(defrule glob-multi-quote
    (or
     (and
      "'"
      (* (and (! "'") character))
      "'")
     (and
      #\"
      (*
       (or glob-single-quote
	   (and (! #\") character)))
      #\"))
  (:destructure
   (start stuff end)
   (declare (ignore start end))
   (mapcar #'second stuff)))

(defrule glob-quote
    (or glob-multi-quote
	glob-single-quote))

(defrule glob-parser
    (*
     (or
      glob-special
      glob-quote-as-sequence
      character))
  (:lambda (stuff)
    `(:sequence :start-anchor ,@stuff :end-anchor)))


#|
(defun unary-op ()
  (=let*
      ((op 
	(=or
	 (=char #\+)
	 (=char #\-)
	 (=char #\~)
	 (=char #\!))))
    (result (make-keyword op))))

(defun unary-and-primary-expr ()
  (=or
   (=let*
       ((unary (unary-op))
	(more (unary-and-primary-expr)))
     (result (list unary more)))
   (primary-expr)))


(defun arithmetic-binop ()
  (=let*
      ((binop
	(=or
	 (=string "<<=")
	 (=string ">>=")
	 (=string "&=")
	 (=string "^=")
	 (=string "|=")
	 (=string "<<")
	 (=string ">>")
	 (=string "<=")
	 (=string ">=")
	 (=string "==")
	 (=string "!=")
	 (=string "&&")
	 (=string "||")
	 (=string "*=")
	 (=string "/=")
	 (=string "%=")
	 (=string "+=")
	 (=string "-=")
	 (=string "=")
	 (=string "&")
	 (=string "^")
	 (=string "|")
	 (=string "*")
	 (=string "/")
	 (=string "%")
	 (=string "+")
	 (=string "-")
	 (=string "<")
	 (=string ">"))))
    (result (make-keyword binop))))

(defun operator-priority (op)
  (case op
    ((:* :/ :%) 1)
    ((:+ :-) 2)
    ((:<< :>>) 3)
    ((:< :<= :> :>=) 4)
    ((:== :!=) 5)
    (:& 6)
    (:^ 7)
    (:\| 8)
    (:&& 9)
    (:\|\| 10)
    ((:= :*= :/= :%= :+= :-=
	 :<<= :>>= :&= :^= :\|=) 11)))

(defun operator-tighter (l r)
  (< (operator-priority l)
     (operator-priority r)))

(defun unwind-op-stack (op-stack left-stack right &optional until)
  (loop 
     for (operator . newop-stack) =  op-stack then newop-stack
     for (left . newleft-stack) = left-stack then newleft-stack
     when (and until
	       (not
		(operator-tighter operator until)))
     return (values
	     right
	     (cons operator newop-stack)
	     (cons left newleft-stack))
     do  (setf right (list operator left right))
     while newop-stack
     finally (return (values right nil nil))))

(defun continue-binop (op-stack left-stack)
  (tracer "C-B"
	  (=let*
	      ((right (unary-and-primary-expr))
	       (op (maybe
		    (arithmetic-binop))))
	    (multiple-value-bind
		  (newleft newop-stack newleft-stack)
		(unwind-op-stack op-stack left-stack right op)
	      (if op
		  (continue-binop
		   (cons op newop-stack)
		   (cons newleft newleft-stack))
		  (result newleft))))))

(defun arithmetic-expr ()
  (=let*
      ((left (unary-and-primary-expr))
       (op
	(tracer "op"
		(maybe
		 (arithmetic-binop)))))
    (if (not op)
	(result left)
	(continue-binop (list op) (list left)))))

(defun literal-number ()
  (=let*
      ((base-marker
	(maybe
	 (string-of
	  (=or
	   (=string "0x")
	   (=prog1
	    (=string "0")
	    (=not (=not
		   (=satisfies
		    (lambda (x) (digit-char-p x))))))))))
       (base
	(cond
	  ((null base-marker) (result 10))
	  ((string= base-marker "0") (result 8))
	  (t (result 16))))
       (number
	(string-of
	 (one-or-more
	  (=satisfies
	   (lambda (x) (digit-char-p x  base)))))))
    (result
     (list :number
	   (parse-integer number :radix base)))))

(defun primary-expr ()
  (=or
   (=let*
       ((_ (=char #\())
	(expr (arithmetic-expr))
	(_ (=char #\))))
     (result expr))
   (=let*
       ((name (string-of (posix-name))))
     (result (list :var name)))
   (literal-number)))

(defun run-arithmetic (pt)
  (let ((op (car pt))
	(left (second pt))
	(right (third pt)))
    (case op
      (:number left)
      (:var (or
	     (let*
		 ((param (plush::get-parameter left))
		  (parsed (when param (funcall (literal-number) param))))
	       (when parsed
		 (destructuring-bind ((val . rest)) parsed
		   (when (emptyp rest) (second val)))))
	     0))
      ((:<<= :>>= :&= :^= :\|= :*= :/=
	     :%= :+= :-=)
       (if (eql :var (car left))
	   (let* ((asnop (symbol-name op))
		  (newop (make-keyword (subseq asnop 0 (1- (length asnop))))))
	     (run-arithmetic `(:= ,left (,newop ,left ,right))))
	   (error "Non-variable on left of assignment")))
      (:<<
       (ash (run-arithmetic left)
	    (run-arithmetic right)))
      (:>>
       (ash (run-arithmetic left)
	    (- (run-arithmetic right))))
      (:<=
       (if (<= (run-arithmetic left)
	       (run-arithmetic right))
	   1 0))
      (:>=
       (if (>= (run-arithmetic left)
	       (run-arithmetic right))
	   1 0))
      (:==
       (if (= (run-arithmetic left)
	      (run-arithmetic right))
	   1 0))
      (:!=
       (if (/= (run-arithmetic left)
	       (run-arithmetic right))
	   1 0))
      (:&&
       (if (and (/= 0 (run-arithmetic left))
		(/= 0 (run-arithmetic right)))
	   1 0))
      (:\|\|
       (if (or (/= 0 (run-arithmetic left))
	       (/= 0 (run-arithmetic right)))
	   1 0))
      (:=
       (if (eql (car left) :var)
	   (setf (iolib/os:environment-variable (second left))
		 (format nil "~D" (run-arithmetic right)))
	   (error "Non variable on left of assignment")))
      (:&
       (logand (run-arithmetic left)
	       (run-arithmetic right)))
      (:^
       (logxor (run-arithmetic left)
	       (run-arithmetic right)))
      (:\|
       (logior (run-arithmetic left)
	       (run-arithmetic right)))
      (:*
       (* (run-arithmetic left)
	  (run-arithmetic right)))
      (:/
       (truncate
	(run-arithmetic left)
	(run-arithmetic right)))
      (:%
       (nth-value
	1
	(truncate
	 (run-arithmetic left)
	 (run-arithmetic right))))
      (:+
       (if right
	   (+
	    (run-arithmetic left)
	    (run-arithmetic right))
	   (run-arithmetic left)))
      (:-
       (if right
	   (-
	    (run-arithmetic left)
	    (run-arithmetic right))
	   (- (run-arithmetic left))))
      (:<
       (if (< (run-arithmetic left)
	      (run-arithmetic right))
	   1 0))
      (:>
       (if (> (run-arithmetic left)
	      (run-arithmetic right))
	   1 0))
      (:~
       (lognot (run-arithmetic left)))
      (:!
       (if (= 0 (run-arithmetic left))
	   1 0)))))

|#
(defvar *preserve-words-as-tokens* nil)
(defvar *alias-stack* nil)

(declaim (optimize (debug 3)))

(define-condition posix-parse-failed (simple-error) ())

#.`(progn
     ,@(loop for item in
	    '("&&" "||" ";;" ">|" ">>" "<&"
	      ">&" "<>" "|" "&" ";"
	      "<" ">" "(" ")"
	      "<<-" "<<")
	    collect
	    `(defun ,(intern (format nil "~:@(operator-~a-p~)" item)) (x)
	       (when
		   (eql (car x) ,(make-keyword item)) x))
	    collect
	    `(esrap:defrule
		 ,(intern (format nil "~:@(<operator-~a>~)"
				  item))
		 (,(intern (format nil "~:@(operator-~a-p~)" item))
		    posix-token)
	       (:identity t))))
   

(defparameter +reserved-words+ '("if" "then" "else" "elif" "fi" "do" "done" "in"
				 "case" "esac" "while" "until" "for" "{" "}" "!"))

#.`(progn
    ,@(loop for item in 
       '("if" "then" "else" "elif" "fi" "do" "done" "in"
	 "case" "esac" "while" "until" "for" "{" "}" "!")
	   collect
	   `(defun
		,(intern (format nil "~:@(reserved-~a-p~)" item))
		(token)
	      (when
		  (and
		   (or (eql (car token) :word)
		       (eql (car token) :token))
		   (string= (second token) ,item))
		token))
	   collect
	   `(esrap:defrule
		,(intern (format nil "~:@(<rword-~a>~)" item))
		(,(intern (format nil "~:@(reserved-~a-p~)" item))
		  posix-token)
	      (:constant
		,(make-keyword (string-upcase item))))))

    
#+(or)(defun tracer (string parser)
  (lambda (input)
    (format t "~A: ~S~%" string input)
    (let ((result (funcall parser input)))
      (format t "~A=>: ~S~%" string result)
      result)))
#-(or)(defun tracer (string parser)
  (declare (ignore string))
  parser)
     

;This needs to be line cached because our
;Tokenizer includes side-effects that disallows backtracking within a line
(defstruct token-source
  (parser nil :type symbol)
  (line-cache nil :type list)
  (input "")
  (position 0))

(defun line-cache-fill (input)
  (when (null (token-source-line-cache input))
    ;(print input)
    (setf (token-source-line-cache input)
	  (loop
	     for (result newstart success) =
	       (multiple-value-list (esrap:parse (token-source-parser input) (token-source-input input)
						 :junk-allowed t :start (token-source-position input)))
	     then
	       (multiple-value-list (esrap:parse (token-source-parser input) (token-source-input input)
						 :start (or newstart (length (token-source-input input))) :junk-allowed t))
	     collect result
	       ;do (print result)
	     while (and
		    (not (eql (car result) :eof))
		    (not (eql (car result) :newline)))
	     finally (setf (token-source-position input)
			   (or newstart (length (token-source-input input)))))))
  ;(print input)
  (token-source-line-cache input))

(defmethod smug::input-first ((input token-source))
  (let
      ((line-cache (line-cache-fill input)))
    (car line-cache)))

(defmethod smug::input-rest ((input token-source))
  (let
      ((line-cache (line-cache-fill input)))
    (make-token-source
     :parser (token-source-parser input)
     :line-cache (cdr line-cache)
     :input (token-source-input input)
     :position (token-source-position input))))

(defmethod smug::input-empty-p ((input token-source))
  (let
      ((line-cache (line-cache-fill input)))
  (eql :eof (caar line-cache))))

(defun convert-maybe-async-list (list)
  (loop for (cmd op . rest) = list then rest
       while cmd
       if (eql (car op) :&)
       collect (list 'plush::asynchronous-cmd cmd)
       else collect cmd))
   
(defrule <eof-complete-command>
    eof-token
  (:identity t))

(defrule <newline-list-complete-command>
    <newline-list>
  (:constant '(())))

(defrule <complete-command-internal>
    (and
     <list>
     (? <separator>))
  (:destructure
   (list op)
   (let
       ((fixed-list (append list (list op))))
      (convert-maybe-async-list fixed-list))))

(defrule <complete-command>
    (or
     <eof-complete-command>
     <newline-list-complete-command>
     <complete-command-internal>)
  (:identity t))


(defun collect-list-and-separators (list-sep last)
  (loop for list = list-sep then (cdr list)
       when list
       append (car list)
       else collect last
       while list))

;TODO alias-stack
(esrap:defrule <list>
    (and
     <and-or>
     (*
      (and
       <separator-op>
       <and-or>)))
  (:destructure
   (first rest)
      (cons first (reduce #'append rest))))
      
(esrap:defrule <and-or>
    (and
     (*
      (and <pipeline>
	   (or <and-if>
	       <or-if>)
	   <linebreak>))
     <pipeline>)
  (:destructure
   (conditionals last)
   (reduce (lambda (x y)
	     (destructuring-bind (pipe op _) x 
	       (declare (ignore _))
	       (if
		(eql (car op) :&&)
		(list 'plush::shell-and pipe y)
		(list 'plush::shell-or pipe y))))
	   conditionals
	   :from-end t :initial-value last)))


(esrap:defrule <pipeline>
  (and
   (? (reserved-!-p posix-word ))
   <pipe-sequence>)
  (:destructure
   (bang sequence)
   (if bang
       `(plush::invert-result ,sequence)
       sequence)))

(esrap:defrule <pipe-sequence>
    (and
     (*
      (and
       <command>
       <vbar>
       <linebreak>))
     <command>)
  (:destructure
   (stuff last)
   `(plush::run-pipe
     ,@(mapcar #'car stuff)
     ,last)))

(defrule <compound-command-and-redirect>
    (and
     <compound-command>
     <redirect-list>)
  (:destructure
   (cmd redir)
   `(plush::compound-command ,cmd ',redir)))

(defrule <command>
    (or
     <function-definition>
     <simple-command>
     <compound-command-and-redirect>)
  (:identity t))
     
(defrule <compound-command>
    (or
     <brace-group>
     <subshell>
     <for-clause>
     <case-clause>
     <if-clause>
     <while-until-clause>)
  (:identity t))

(defrule <subshell>
    (and
     <open-paren>
     <compound-list>
     <close-paren>)
  (:destructure
   (open list close)
   (declare (ignore open close))
   `(plush::subshell-group ',list)))
		
(defrule <compound-list>
    (and
     (? <newline-list>)
     (or
      (and
       (+
	(and
	 <and-or>
	 <separator>))
       (? <and-or>))
      (and
       <and-or>)))
  (:destructure
   (_ stuff)
   (declare (ignore _))
   (let ((stuff (car (butlast stuff)))
	 (last (car (last stuff))))
   (printv stuff)
   (printv last)
     (convert-maybe-async-list
      (if last
	  (collect-list-and-separators stuff last)
	  (mapcan #'copy-list stuff))))))

(defrule <for-clause>
    (and
     (reserved-for-p posix-word)
     <name>
     <linebreak>
     (?
      (and
       (reserved-in-p posix-token)
       (? <wordlist>)
       <sequential-sep>))
     <do-group>)
  (:destructure
   (for name lb in do-group)
   (declare (ignore for lb))
   (let ((wordlist
	  (if in
	      (second in)
	      "\"$@\"")))
     `(plush::posix-for ,(second name) (plush::expand-word-list ,`(quote ,wordlist)) ,`(quote ,do-group)))))

(defun posix-name-p (token)
  (when
      (and (eql :token (car token))
	   (handler-case
	       (esrap:parse 'posix-name (second token))
	     (t nil)))
    token))

(defrule <name>
    (posix-name-p posix-token)
  (:lambda (x)
    (list :name (second x))))
    
(defrule <wordlist>
    (+ <word>)
  (:lambda (words)
    (mapcar #'second words)))

(defun reserved-word-p (token)
  (when
      (and
       (or
	(eql (car token) :token)
	(eql (car token) :word))
       (member (second token) +reserved-words+ :test #'equal))
    token))

(defun not-reserved-word-p (token)
  (when
      (not (reserved-word-p token))
    token))

(defun wordp (token)
  (when
      (eql (car token) :token)
    token))

(defrule <word-norsvd>
    (not-reserved-word-p <word>))

(defrule <word>
    (wordp posix-token)
  (:lambda (word)
    (if *preserve-words-as-tokens*
	word
	(list :word (second word)))))

(defrule <case-subclause>
    (and
     (? <operator-\(>)
     <pattern>
     <operator-\)>
     (or
      <compound-list>
      <linebreak>)
     <operator-|;;|>
     <linebreak>)
     (:destructure
      (open pattern close meat sep lb)
      (declare (ignore open close sep lb))
      (list pattern meat)))

(defrule <last-case-subclause>
    (and
     (? <operator-\(>)
     <pattern>
     <operator-\)>
     <compound-list>
     <linebreak>)
  (:destructure
   (open pattern close meat lb)
   (declare (ignore open close lb))
   (list pattern meat)))

(defrule <case-clause>
    (and
     (reserved-case-p posix-token)
     <word>
     <linebreak>
     (reserved-in-p posix-token)
     <linebreak>
     (* <case-subclause>)
     (? <last-case-subclause>))
  (:destructure
   (case word lb in lb2 stuff last)
   (declare (ignore case lb in lb2))
   `(plush::posix-case ,(second word) ,@stuff ,@(when last `(,last)))))

(defun not-reserved-esac-p (token)
  (when
      (not (reserved-esac-p token))
    token))

(defrule <pattern>
    (and
     (not-reserved-esac-p <word>)
     (*
      (and
       <operator-\|>
       <word>)))
  (:destructure
   (first rest)
   (mapcar #'second (cons first rest))))
   
(defrule <if-clause>
    (and
     (reserved-if-p posix-token)
     <compound-list>
     (reserved-then-p posix-token)
     <compound-list>
     <else-part>
     (reserved-fi-p posix-token))
  (:destructure
   (if if-part then then-part else-part fi)
   (declare (ignore if then fi))
   (printv (values if-part then-part else-part))
   `(plush::posix-if ,`(quote ,if-part)
		     ,`(quote ,then-part)
		     ,`(quote ,else-part))))

(defrule <else-part>
   (or 
    <elif-rule>
    <else-rule>))

(defrule <elif-rule>
    (and
     <rword-elif>
     <compound-list>
     <rword-then>
     <compound-list>
     <else-part>)
  (:destructure
   (elif elif-part then then-part else-part)
   (declare (ignore elif then))
   `((plush::run-pipe
     (plush::posix-if ,`(quote ,elif-part)
		      ,`(quote ,then-part)
		      ,`(quote ,else-part))))))

(defrule <else-rule>
    (?
     (and <rword-else>
	  <compound-list>))
  (:function second))

(defrule <while-until-clause>
    (and
     (or <rword-until>
	 <rword-while>)
     <compound-list>
     <do-group>)
  (:destructure
   (type while do)
   `(plush::posix-while-until ,`',while ,`',do ,(eql type :until))))

(defrule <function-definition>
    (and
     <name>
     <operator-\(>
     <operator-\)>
     <linebreak>
     <compound-command>
     (? <redirect-list>))
  (:destructure
   (fname open close lb body redirect)
   (declare (ignore open close lb))
   `(plush::define-function ,(second fname) ',body ',redirect))) 

(defrule <brace-group>
    (and
     <rword-{>
     <compound-list>
     <rword-}>)
  (:destructure
   (open cmd close)
   (declare (ignore open close))
   `(plush::brace-group ',cmd)))

(defrule <do-group>
    (and
     <rword-do>
     <compound-list>
     <rword-done>)
  (:function second))

;;BUG TODO UGLY DAMNIT POSIX
;;This can backtrack, potentially across a HERE-DOC
;;That breaks my ugly hack for newline-skip :(

(defun not-null (x)
  (not (null x)))

(defrule <cmd-helper>
    (or
     (and
      (not-null <cmd-prefix>)
      (? <word>))
     (and
      <word-norsvd>))
  (:lambda (x)
    (if (= (length x) 1)
	(cons nil x)
	x)))
      
;todo expand next word when alias ends in space
(defun alias-command-fn (input start end)
  (let
      ((stuff
	(let (
	      (*preserve-words-as-tokens* t))
	  (esrap:parse
	   '<cmd-helper> input :start start :end end :junk-allowed t))))
    (if stuff
      (let* ((prefix (car stuff))
	     (cmd (cadr stuff))
	     (alias (and (not (member (second cmd) *alias-stack* :test #'string=))
			 (plush::alias-substitute (second cmd)))))
	(if alias
	  (let ((backtrack-to start)
		  (newline-skip (get-newline-skip-for-position start))
		  (cmd-start (third cmd))
		  (cmd-end (fourth cmd)))
	      (let ((*alias-stack*
		     (cons (second cmd) *alias-stack*)))
		(destructuring-bind
		      (stuff next &optional (info :not-set)
			     &aux (success
				   (or (and (eql info :not-set) (> next 0))
				       (eql info t))))
		    (let ((*newline-skip-list* (list (cons 0 newline-skip))))
		      (multiple-value-list
		       (esrap::parse '<command>
				     (concatenate 'string
						  (subseq input
							  backtrack-to cmd-start)
						  alias
						  (subseq input
							  cmd-end))
				     :junk-allowed t)))
		  
		  (if success
		      (values stuff
			      (- (+ next backtrack-to (- cmd-end cmd-start)) (length alias))
			      t)
		      (values nil 0)))))
	  (values nil start)))
      (values nil start))))

(defrule <simple-command>
    (or
     (function alias-command-fn)
     <truely-simple-command>)
    (:identity t))

(defun cmd-has-meat-p (cmd)
  (when (second cmd)
    cmd))

(defrule <truely-simple-command> 
  (or
   (and
    (cmd-has-meat-p <cmd-helper>)
    <cmd-suffix>)
   (and
    <cmd-helper>))
  (:destructure
   ((prefix cmd) &optional suffix)
   (let
       ((assignments
	 (loop for item in prefix
	    when (eql (car item) :assignment)
	    collect (cadadr item)))
	(redirects
	 (append
	  (loop for item in prefix
	     when (eql (car item) :io-redirect)
	     collect (second item))
	  (loop for item in suffix
	     when (eql (car item) :io-redirect)
	     collect (second item))))
	(other-words
	 (loop for item in suffix
	    when (eql (car item) :word)
	    collect (second item))))
     `(plush::simple-command
       (plush::expand-word-list (quote ,(append (cdr cmd) other-words)))
       (quote ,redirects)
       (plush::expand-word-list (quote ,assignments)
				:split-fields nil
				:variable-assignment t)))))


(defrule <cmd-prefix>
    (*
     (or
      <io-redirect>
      <assignment-word>))
  (:identity t))

(defun assignment-word-p (word)
  (when
   (let ((pos (position #\= (second word))))
     (and pos
	  (not (char= (elt (second word) 0) #\=))
	  (handler-case
	      (esrap:parse 'posix-name (second word) :end pos)
	    (t nil))))
    word))

(defrule <assignment-word>
    (assignment-word-p <word>)
  (:lambda (word)
    (if *preserve-words-as-tokens*
	(list word)
	(list :assignment word))))

(defrule <cmd-suffix>
    (*
     (or <io-redirect>
	 <word>))
  (:identity t))

(defrule <redirect-list>
    (* <io-redirect>)
  (:lambda (list)
    (mapcar #'second list)))

(defrule <io-redirect>
    (and
     (? io-number)
     (or
      <io-file>
      <io-here>))
  (:destructure
   (number to)
   (if *preserve-words-as-tokens*
       (append number to)
       (list :io-redirect
	     (append to (list (when number (parse-integer (second number)))))))))
		
(defrule <io-file>
    (and
     (or
      <operator-<>
      <operator-&>
      <operator->>
      <operator->&>
      <operator->>>
      <operator-<>>
      <operator->\|>)
     <word>)
  (:destructure
   (op filename)
   (if *preserve-words-as-tokens*
       (list op filename)
       (list :io-file (car op) (second filename)))))

(defrule <io-here>
    (and
     (or
      <operator-<<>
      <operator-<<->)
     <word>)
  (:destructure
   (op here-end)
    `(:io-here ,(car op)
	       (,(plush::unquote (second here-end))
		 ,(if (string= (plush::unquote (second here-end))
			       (second here-end))
		      `(plush::expand-here-doc
			    ,(second (third op)))
		      (second (third op)))))))

(defrule <newline>
    newline-token
  (:lambda (x)
    x))

(defrule <newline-list>
    (+ <newline>)
  (:identity t))
    
(defrule <linebreak>
    (? <newline-list>))

(defun separator-op-p (x)
    (or (operator-&-p x)
	(operator-\;-p x)))

(defrule <separator-op>
    (separator-op-p
     posix-token))

(defrule <separator>
    (or
     (and <separator-op>
	  <linebreak>)
     (and <newline-list>))
  (:lambda (x) (car x)))

(defrule <sequential-sep>
    (or
     (and
      <operator-\;>
      <linebreak>)
     (and
      <newline-list>))
  (:lambda (x) (car x)))

(defmacro rule-alias (to from)
  `(esrap:defrule ,to ,from (:identity t)))

(rule-alias <and-if> <operator-&&>)
(rule-alias <open-paren> <operator-\(>)
(rule-alias <close-paren> <operator-\)>)
(rule-alias <vbar> <operator-\|>)
(rule-alias <or-if> <operator-\|\|>)


(defun parse-one-command (input &optional (start 0))
  (multiple-value-bind
	(parsed next-position)
      (esrap:parse '<complete-command> input :start start :junk-allowed t)
    (if parsed
	(values parsed next-position)
	(error (make-condition 'posix-parse-failed :format-control "Failed to parse")))))
  
(defun parse-posix-stuff (input &optional (start 0))
  (let  ((*newline-skip-list* (list (cons 0 0))))
    (loop
       for position  = start then rest
       for (result rest ) =
	 (multiple-value-list (esrap:parse '<complete-command> input :start position :junk-allowed t))
					;do (format *error-output* "~A" parsed-input)
					;do (format t "~&RESULT: ~s~%" result)
       when (not result)
       do (error (make-condition 'posix-parse-failed :format-control "Failed to parse"))
       append result
       while (and rest (not (eql (car result) :eof))))))
