(in-package :plush-token)

(declaim (optimize (debug 3)))

(define-condition eof-when-tokenizing (simple-error) ())


(defparameter *newline-skip* 0
  "Number of characters to skip when next newline is encountered

This must be reinitilized to the fifth value of the token list when
retokenizing (e.g. after an alias is expanded))

To see why,consider:

    foo <<EOF; someAlias <<EOF
    a
    EOF
    b
    EOF

someAlias will tokenize as (:token \"someAlias\" 11 19 5) since we
need to skip 5 characters to consume the first here document.  So when
we alias expand someAlias and restart the tokenizing from there, it is necessary to set *newline-skip* to 5")


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



;TODO esrap set newline-=skip
(defun here-doc-internal (input start end)
  ;(format t "HDI: ~a ~a ~a~%" input start end)
  (multiple-value-bind (word word-end info)
    (esrap:parse 'posix-word input :start start :end end :junk-allowed t)
      (declare (ignorable info))
    ;(format t "HDI: ~a ~a ~a~%" word word-end info)
    (if (= word-end start)
      (values nil nil "Error parsing here-doc")
      (multiple-value-bind
	    (doc newend success)
	     (esrap:parse
	      `(and
		(* (and (! #\Newline) character))
		#\Newline
		(string ,*newline-skip*)
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
	    (values nil nil "Error parsing here-doc")
	    (let ((text (text (nthcdr 3 doc))))
	      (incf *newline-skip* (length text))
	      (values (list :here-doc (subseq text 0 (- (length text) (length word)))) end t))))
	  )))

    
;This has a side-effect and so relies on not-backtracking
;Since operators are unambiguous that should be fine

(defrule here-doc-reader
   (&
    (function here-doc-internal)))

(defrule here-doc-op
     (and
       (or "<<-" "<<")
       (? here-doc-reader))
     (:destructure (op doc)
                   (list (make-keyword op) op doc)))


(defrule operator-start
   (or "|" "&" ";" "<" ">" "(" ")"))


(defrule operator-token
  (or here-doc-op
      "&&" "||" ";;" ">>" "<&"
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
  (when (>= end (+ start *newline-skip*))
   (values nil (+ start (shiftf *newline-skip* 0)) t)))

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
    (append x (list start end *newline-skip*))))

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
      charater))
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
(in-package :plush-parser)

(declaim (optimize (debug 3)))

(define-condition posix-parse-failed (simple-error) ())

(defparameter +reserved-words+ '("if" "then" "else" "elif" "fi" "do" "done" "in"
				 "case" "esac" "while" "until" "for" "{" "}" "!"))

(defparameter *newline-skip* 0)
    
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
   
(defun <complete-command> (alias-stack)
  (tracer "CC"
  (=or
   (=not (item))
   (=satisfies (lambda (x) (and (consp x) (eql (car x) :eof))))
   (=let* ((_ (<newline-list>)))
     (result
      '(())))
   (=let*
	((list (<list> alias-stack))
	(op (maybe (<separator>))))
     (let
	 ((fixed-list (append list (list op))))
       (result
	(convert-maybe-async-list fixed-list)))))))

(defun collect-list-and-separators (list-sep last)
  (loop for list = list-sep then (cdr list)
       when list
       append (car list)
       else collect last
       while list))

(defun <list> (alias-stack)
  (=let*
      ((first (<and-or> alias-stack))
       (rest
	(zero-or-more
	 (=let* ((op (<separator-op>))
		 (andor (<and-or> alias-stack)))
	   (result (list op andor))))))
    (result (cons first (reduce #'append rest)))))

(defun <and-or> (alias-stack)
  (=let*
      ((conditionals
	(zero-or-more
	 (=let*
	     ((pipe (<pipeline> alias-stack))
	      (op
	       (=or (<and-if>)
		    (<or-if>)))
	      (_ (<linebreak>)))
	   (result
	    (list pipe op)))))
       (last (<pipeline> alias-stack)))
    (result
     (reduce (lambda (x y)
	       (destructuring-bind (pipe op) x
		 (if
		  (eql (car op) :&&)
		       (list 'plush::shell-and pipe y)
		       (list 'plush::shell-or pipe y))))
		  conditionals
	       :from-end t :initial-value last))))

(defun <pipeline> (alias-stack)
    (=let*
	((bang (maybe (rword :!)))
	 (sequence (<pipe-sequence> alias-stack)))
      (result
       (if bang
	   `(plush::invert-result ,sequence)
	   sequence))))

(defun <pipe-sequence> (alias-stack)
  (=let*
      ((stuff
	(zero-or-more
	 (=prog1
	  (<command> alias-stack)
	  (<vbar>)
	  (<linebreak>))))
       (last (<command> alias-stack)))
    (result
     `(plush::run-pipe
	     ,@stuff
	     ,last))))

(defun <command> (alias-stack)
  (=or
   (<function-definition> alias-stack)
   (<simple-command> alias-stack)
   (=let*
       ((cmd (<compound-command> alias-stack))
	(redir (maybe (<redirect-list>))))
     (result `(plush::compound-command ,cmd ',redir)))))

(defun <compound-command> (alias-stack)
  (=let*
      ((cmd
	(=or (<brace-group> alias-stack)
	     (<subshell> alias-stack)
	     (<for-clause> alias-stack)
	     (<case-clause> alias-stack)
	     (<if-clause> alias-stack)
	     (<while-until-clause> alias-stack))))
    (result
     cmd)))

(defun <subshell> (alias-stack)
  (=let*
      ((_ (<open-paren>))
       (list (<compound-list> alias-stack))
       (_ (<close-paren>)))
    (result
     (list 'plush::with-subshell '(t) list))))

(defun <compound-list> (alias-stack )
  (=let*
      ((_ (maybe (<newline-list>)))
       (stuff
	(zero-or-more
	 (=let*
	     ((item (<and-or> alias-stack))
	      (sep (<separator>)))
	   (result (list item sep)))))
       (last
	(if (zerop (length stuff))
	    (<and-or> alias-stack)
	    (maybe (<and-or> alias-stack)))))
    (result
     (convert-maybe-async-list
      (if last
	  (collect-list-and-separators stuff last)
	  (mapcan #'copy-list stuff))))))

(defun <for-clause> (alias-stack)
  (=let*
      ((_ (rword :for))
       (name (<name>))
       (_ (<linebreak>))
       (in (maybe (rword :in)))
       (wordlist
	(if in
	    (maybe (<wordlist>))
	    (result nil)))
       (_ (if in (<sequential-sep>)
	      (result t)))
       (do-group (<do-group> alias-stack)))
    (let
	((wordlist (if in wordlist '("$@"))))
      (result
       `(plush::posix-for ,(second name) (plush::expand-word-list ,`(quote ,wordlist)) ,`(quote ,do-group))))))

(defun <name> ()
  (=let*
   ((name (=satisfies (lambda (x)
			(and (eql :token (car x))
			     (handler-case
				 (esrap:parse 'plush-token::posix-name (second x))
			       (t nil)))))))
    (result (list :name (second name)))))


(defun <wordlist> ()
  (=let* ((words (one-or-more (<word>))))
    (result (mapcar #'second words))))

(defun <word> (&optional disallowed-rsvd alias-mode)
  (=let*
      ((word (=satisfies (compose (curry #'eql :token) #'car))))
    (if
     (member (second word) disallowed-rsvd :test #'string=)
     (fail)
     (result (if alias-mode
		 word
		 (list :word (second word)))))))

(defun <case-clause> ( alias-stack)
  (=let*
      ((_ (rword :case))
       (word (<word>))
       (_ (<linebreak>))
       (_ (rword :in))
       (_ (<linebreak>))
       (stuff
	(zero-or-more
	 (=let*
	     ((_ (maybe (op #\()))
	      (pattern (<pattern>))
	      (_ (op #\)))
	      (meat (=or
		     (<compound-list> alias-stack)
		     (<linebreak>)))
	      (_ (op ";;"))
	      (_ (<linebreak>)))
	   (result (list pattern meat)))))
       (last
	(maybe
	 (=let*
	     ((_ (maybe (op #\()))
	      (pattern (<pattern>))
	      (_ (op #\)))
	      (meat (maybe (<compound-list> alias-stack)))
	      (_ (<linebreak>)))
	   (result (list pattern meat)))))
       (_ (rword :esac)))
    (result
     `(plush::posix-case ,(second word) ,@stuff ,@(when last `(,last))))))

(defun <pattern> ()
  (=let*
      ((_ (=not (rword :esac)))
       (first (<word>))
       (rest
	(zero-or-more
	 (=and
	  (op "|")
	  (<word>)))))
    (result
     (mapcar #'second (cons first rest)))))

(defun <if-clause> (alias-stack)
  (=let*
      ((_ (rword :if))
       (if-part (<compound-list> alias-stack))
       (_ (rword :then))
       (then-part (<compound-list> alias-stack))
       (else-part (<else-part> alias-stack))
       (_ (rword :fi)))
    (result `(plush::posix-if ,`(quote ,if-part)
			      ,`(quote ,then-part)
			      ,`(quote ,else-part)))))

(defun <else-part> (alias-stack)
  (=or
   (=let*
       ((_ (rword :elif))
	(elif-part (<compound-list> alias-stack))
	(_ (rword :then))
	(then-part (<compound-list> alias-stack))
	(else-part (<else-part> alias-stack)))
     (result `((plush::run-pipe
		(plush::posix-if ,`(quote ,elif-part)
			       ,`(quote ,then-part)
			       ,`(quote ,else-part))))))
   (=let*
       ((else
	 (maybe
	  (=and
	   (rword :else)
	   (<compound-list> alias-stack)))))
     (result 
      else))))

(defun <while-until-clause> (alias-stack)
  (=let*
      ((type
	(=or
	 (rword :while)
	 (rword :until)))
       (while (<compound-list> alias-stack))
       (do (<do-group> alias-stack)))
    (result `(plush::posix-while-until ,`',while ,`',do ,(eql type :until)))))

(defun <function-definition> (alias-stack)
  (=let*
      ((fname (tracer "FNAME" (<name>)))
       (_ (op "("))
       (_ (op ")"))
       (_ (<linebreak>))
       (body (tracer "FBODY" (<compound-command> alias-stack)))
       (redirect (maybe (<redirect-list>))))
    (result `(plush::define-function ,(second fname) ',body ',redirect))))

(defun <brace-group> (alias-stack)
  (=let*
      ((_ (rword :{))
       (cmd (<compound-list> alias-stack))
       (_ (rword :})))
    (result `(plush::brace-group ',cmd))))

(defun <do-group> (alias-stack)
  (=prog2
   (rword :do)
   (<compound-list> alias-stack)
   (rword :done)))

;;BUG TODO UGLY DAMNIT POSIX
;;This can backtrack, potentially across a HERE-DOC
;;That breaks my ugly hack for newline-skip :(

(defun <alias-command> (alias-stack)
  (lambda (input)
    (let ((stuff (funcall
		  (=let*
		      ((prefix (tracer "PRE" (<cmd-prefix> t)))
		       (cmd (if prefix
				(maybe (<word> nil t))
				(<word> +reserved-words+ t))))
		    ;(format t "~%PREFIX: ~A~%" prefix)
		    ;(format t "~%PREFIX2: ~A~%" (mapcar #'append prefix))
		    (result (cons (reduce #'append prefix) cmd)))
		  input)))
      (when input
	(let* ((prefix (caaar stuff))
	       (cmd (cdaar stuff))
	       (rest (cdar stuff))
	       (input-string (token-source-input input))
	       (alias (and (not (member (second cmd) alias-stack :test #'string=))
			   (plush::alias-substitute (second cmd)))))
	  (declare (ignorable rest))
	  (if alias
	      (let ((backtrack-to
		     (or
		      (and prefix (third (car prefix)))
		      (third cmd)))
		    (newline-skip
		     (or
		      (and prefix (fifth (car prefix)))
		      (fifth cmd)))
		    (cmd-start (third cmd))
		    (cmd-end (fourth cmd)))
	    (progn
	      ;(format t "Alias-test: ~S ~S ~S~%" prefix cmd rest)
	      (setf plush-token::*newline-skip* newline-skip)
	      (funcall (<command> (cons (second cmd) alias-stack))
		       (make-token-source
			:parser (token-source-parser input)
			:line-cache nil
			:position 0
			:input
			 (concatenate 'string
					      (subseq input-string
						      backtrack-to cmd-start)
					      alias
					      (subseq input-string
						      cmd-end))))))))))))
      
      
(defun <simple-command> (alias-stack)
  (=or (<alias-command> alias-stack)
       (<truely-simple-command>)))

(defun <truely-simple-command> ()
  (=let*
      ((prefix (tracer "TS-PRE" (<cmd-prefix>)))
       (cmd
	(if prefix
	    (maybe (<word>))
	    (<word> +reserved-words+)))
       (suffix (if cmd
		   (<cmd-suffix>)
		   (result nil))))
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
					;(format t "~S~%" prefix)
					;(if cmd
      (result `(plush::simple-command
		(plush::expand-word-list (quote ,(append (cdr cmd) other-words)))
		
		(quote ,redirects)
		(plush::expand-word-list (quote ,assignments)
					 :split-fields nil
					 :variable-assignment t))))))
	  ;(result `(plush::set-vars (quote ,assignments)))))))

(defun <cmd-prefix> (&optional alias-mode)
  (zero-or-more
   (=or
    (<io-redirect> alias-mode)
    (<assignment-word> alias-mode))))

(defun <assignment-word> (&optional alias-mode)
  (=let*
      ((word (<word> nil alias-mode)))
    (if
     (let ((pos (position #\= (second word))))
     (and pos
	  (not (char= (elt (second word) 0) #\=))
	  (handler-case
	      (esrap:parse 'plush-token::posix-name (second word) :end pos)
	    (t nil))))
     (result
      (if alias-mode
	  (list word)
	  (list :assignment word)))
     (fail))))
      
(defun <cmd-suffix> ()
  (zero-or-more
   (=or
    (<io-redirect>)
    (<word>))))

(defun <redirect-list> ()
  (=let* ((list (one-or-more (<io-redirect>))))
    (result (mapcar #'second list))))

(defun <io-redirect> (&optional alias-mode)
  (=let*
      ((number (maybe (=satisfies (lambda (x) (eql (car x) :io-number)))))
       (to
	(=or
	 (<io-file> (second number) alias-mode)
	 (tracer "IOHERE"
	 (<io-here> (second number) alias-mode)))))
    (result 
     (if alias-mode
	 (append number to)
	 (list :io-redirect to)))))

(defun <io-file> (number &optional alias-mode)
  (=let*
      ((op
	(=or
	 (op "<")
	 (op "<&")
	 (op ">")
	 (op ">&")
	 (op ">>")
	 (op "<>")
	 (op ">|")))
       (filename (<word> nil alias-mode)))
    (result
     (if alias-mode
	 (list op filename)
	 (list :io-file (car op) (second filename)
	       (when number (parse-integer number)))))))

(defun <io-here> (number &optional alias-mode)
  (=let*
      ((op
	(=or
	 (op "<<")
	 (op "<<-")))
       (here-end (<word> nil alias-mode)))
    (result
     (if alias-mode
	 (list op here-end)
	 `(:io-here ,(car op)
		    (,(plush::unquote (second here-end))
		      ,(if (string= (plush::unquote (second here-end))
				    (second here-end))
			   (list 'plush::expand-here-doc
				 (second (third op)))
			   (second (third op))))
		    ,(when number (parse-integer number)))))))

(defun <newline> ()
   (=satisfies (lambda (x) (eql (car x) :newline))))

(defun <newline-list> ()
  (one-or-more
   (<newline>)))

(defun <linebreak> ()
  (maybe (<newline-list>)))

(defun <separator-op> ()
    (=or
     (op "&")
     (op ";")))

(defun <separator> ()
  (=or
   (=prog1
    (<separator-op>)
    (<linebreak>))
   (<newline-list>)))

(defun <sequential-sep> ()
  (=or
   (=prog1
    (op ";")
    (<linebreak>))
   (<newline-list>)))

(defun op (op)
  (=satisfies
   (compose
    (curry #'eql (make-keyword (string op)))
    #'car)))

(defun rword (word)
  (=let*
      ((rword (=satisfies
	       (lambda (x)
		 (and
		  (eql (car x) :token)
		  (string= (string-downcase (symbol-name word)) (second x)))))))
    (result (make-keyword (string-upcase (second rword))))))

(defun <and-if> ()
  (=satisfies (compose (curry #'eql :&&) #'car) ))

(defun <open-paren> ()
  (=satisfies (compose (curry #'eql :\() #'car) ))

(defun <close-paren> ()
  (=satisfies (compose (curry #'eql :\)) #'car) ))

(defun <vbar> ()
  (=satisfies (compose (curry #'eql :\|) #'car) ))

(defun <or-if> ()
  (=satisfies (compose (curry #'eql :\|\|) #'car) ))
    
#+(or)(defun get-another-command (rest)
  (destructuring-bind (&optional result) 
      (funcall (<complete-command> nil) rest)
    (when result
	(values (car result)
		(cdr result)))))

(defun parse-one-command (input)
  (let* ((source (make-token-source :parser 'plush-token::posix-token
				    :input input))
	 (parsed-input (funcall (<complete-command> nil) source)))
    (if parsed-input
	(destructuring-bind ((parsed . rest)) parsed-input
	  (values parsed (token-source-position rest)))
	(error (make-condition 'posix-parse-failed :format-control "Failed to parse")))))
	
  
(defun parse-posix-stuff (input)
  (let* ((source (make-token-source :parser 'plush-token::posix-token
				  :input input)))
    (loop
       for input  = source then rest
       for parsed-input = (funcall (<complete-command> nil) input)
	 for ((result . rest)) = parsed-input
	 ;do (format *error-output* "~A" parsed-input)
	 when (not parsed-input)
	 do (error (make-condition 'posix-parse-failed :format-control "Failed to parse"))
	 append result
	 while (not (smug::input-empty-p rest)))))
