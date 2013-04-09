(in-package :plush-parser)

(declaim (optimize (debug 3)))

(defparameter +reserved-words+ '("if" "then" "else" "elif" "fi" "do" "done" "in"
				 "case" "esac" "while" "until" "for" "{" "}" "!"))

(defparameter *newline-skip* 0)

(defun eof-token ()
  (=let* ((_ (=not (item))))
    (result (list :eof))))

(defun peek (parser)
  (lambda (input)
    (mapcar (lambda (x) (cons (car x) input))
	    (funcall parser input))))

(define-condition eof-when-tokenizing (simple-error) ())

(define-condition posix-parse-failed (simple-error) ())

;This has a side-effect and so relies on not-backtracking
;Since operators are unambiguous that should be fine
(defun here-doc-internal ()
  (=let*
      ((word (posix-word)))
    (=or
     (=let*
	 ((_ (zero-or-more
	      (=satisfies (curry #'char/= #\Newline))))
	  (_ (exactly *newline-skip* (item))))
       (let* ((word (plush::unquote (second word)))
	      (terminator (format nil "~C~A~C" #\Newline word #\Newline)))
	 (tracer (format nil "HDI ~A" word)
		 (=let*
		     ((stuff
		       (zero-or-more
			(=and
			 (=not (=string terminator))
			 (item))))
		      (_ (=string terminator)))
		   (result
		    (progn
		      ;(format t "Newline-skip == ~S~%" *newline-skip*)
		      (incf *newline-skip*
			    (1- (+ (length stuff) (length terminator))))
		      ;(format t "Newline-skip <- ~S~%" *newline-skip*)
		      (list :here-doc
			    (if stuff
				(coerce
				 (append (cdr stuff) (list (car stuff))) 'string)
				""))))))))
     (=let* ((_ (result t)))
      (error (make-condition 'eof-when-tokenizing :format-control "EOF looking for ~A"
			     :format-arguments (cdr word)))))))

(defun here-doc-reader ()
   (peek
    (here-doc-internal)))

(defun here-doc-op ()
  (tracer "HDO"
  (=let*
      ((operator
	(=or
	 (=string "<<-")
	 (=string "<<")))
       (doc (maybe (here-doc-reader))))
    (result
     (list (make-keyword operator) operator doc)))))

(defun operator-start ()
  (=or
   (=satisfies (rcurry #'position "|&;<>()"))))
  
(defun operator-token ()
  (=or
   (here-doc-op)
   (=let*
       ((operator
	 (=or
	  (=string "&&")
	  (=string "||")
	  (=string ";;")
	  (=string ">>")
	  (=string "<&")
	  (=string ">&")
	  (=string "<>")
	  (=satisfies (rcurry #'position "|&;<>()"))
	  )))
     (result
      (list (make-keyword operator) operator)))))

(defun line-comment ()
  (=and (=char #\#)
	(zero-or-more
	 (=satisfies
	  (lambda (x) (char/= #\Newline x))))))

(defun newline-token ()
  (=let* ((_ (=char #\Newline))
	  (_ (exactly *newline-skip* (item))))
    (result
     (progn
       (setf *newline-skip* 0)
       ;(format t "Newline-skip <- ~S~%" *newline-skip*)
    (list :newline #.(make-string 1 :initial-element #\Newline))))))

(defun io-number ()
  (=let* ((num (string-of (one-or-more (=satisfies #'digit-char-p))))
	  (_ (=not (=not (=or (=char #\<) (=char #\>))))))
    (result (list :io-number num))))

(defun blank-character ()
  ;TODO Locale stuff, right now everything is POSIX locale
  (=or
   (=char #\Space)
   ;(=char #\Newline) Newlines are special in the tokenizer
   (=char #\Page)
   (=char #\Return)
   (=char #\Tab)
   (=char #\Vt)))

(defun backslash-quote ()
  (=let*
      ((_ (=char #\\))
       (c (item)))
    (result
     (when (char/= c #\Newline)
       (format nil "\\~C" c)))))

(defun double-quoted-meat ()
  (=and
   (=char #\")
   (=or
    (=let*
	((stuff
	  (zero-or-more
	   (=and
	    (=not (=char #\"))
	    (=or
	     (backslash-quote)
	     (arithmetic-expansion)
	     (command-substitution)
	     (parameter-expansion)
	     (item)))))
	 (_ (=char #\")))
      (result (format nil "\"~{~A~}\"" stuff)))
    (=let* ((_ (result t)))
      (error (make-condition 'eof-when-tokenizing :format-control "EOF looking for \""))))))

(defun single-quoted-meat ()
  (=and
   (=char #\')
   (=or
    (=let*
	((stuff
	  (string-of
	   (zero-or-more
	    (=satisfies (curry #'char/= #\')))))
	 (_ (=char #\')))
      (result (format nil "'~A'" stuff)))
    (=let* ((_ (result t)))
      (error (make-condition 'eof-when-tokenizing :format-control "EOF looking for '"))))))

(defun parenthesized-expression (parser)
  (=let*
      ((_ (=char #\())
       (stuff (zero-or-more
	       (=and (=not (=char #\)))
		     parser)))
       (_ (=char #\))))
    (result (format nil "(~{~A~})" stuff))))
	  
(defun arithmetic-expansion ()
  (=let*
   ((_ (=string "$(("))
    (stuff
     (zero-or-more
      (=and
       (=not (=string "))"))
       (token-string-until-unbalanced-paren))))
       (_ (=string "))")))
    (result
     (format nil "$((~{~{~A~}~}))" stuff))))

(defun command-substitution ()
  (=or
   (=let*
       ((_ (=char #\`))
	(stuff
	 (zero-or-more
	  (=or
	   (=and
	    (=char #\\)
	    (=or
	     (=and (=char #\$) (result "\$"))
	     (=and (=char #\`) (result "\`"))
	     (=and (=char #\\) (result "\\"))
	     (result #\\)))
	   (=satisfies (curry #'char/= #\`)))))
	(_ (=char #\`)))
     (result (format nil "`~{~A~}`" stuff)))
   (=let*
       ((_ (=string "$("))
	(tokens
	 (token-string-until-unbalanced-paren))
	(_ (=string ")")))
     (result 
     (format nil "$(~A)" tokens)))))
	 
(defun token-or-pair (start end)
  (=let*
      ((much-stuff
	(zero-or-more
	 (tracer "much-stuff"
	 (=or
	  (=let* ((_ (=not (item))))
	    (error (make-condition 'eof-when-tokenizing :format-control "EOF looking for ~A" :format-arguments (list end))))
	  (=and
	   (=not (=char end))
	   (=or
	    (=let*
		((_ (=char start))
		 (stuff
		  (tracer "STUFF"
		  (token-or-pair start end)))
		 (_ (=char end)))
	      (if stuff
		  (result (format nil "~C~A~C" start stuff end))
		  (result (coerce (list start end) 'string))))
	    (token-meat)
	    (item))))))))
    (result 
    (apply #'concatenate 'string (mapcar #'string much-stuff)))))

(defun token-string-until-unbalanced-brace ()
  (token-or-pair #\{ #\}))
 
(defun token-string-until-unbalanced-paren ()
  (token-or-pair #\( #\)))

(defun posix-name ()
  (=let*
      ((first-char (=satisfies (rcurry #'position "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")))
       (more-chars
	(zero-or-more
	 (=satisfies
	  (rcurry #'position "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_1234567890")))))
    (result (cons first-char more-chars))))
    
#-(or)(defun tracer (string parser)
  (lambda (input)
    (format t "~A: ~S~%" string input)
    (let ((result (funcall parser input)))
      (format t "~A=>: ~S~%" string result)
      result)))
#+(or)(defun tracer (string parser)
  (declare (ignore string))
  parser)

(defun parameter-expansion ()
  (=and
   (=char #\$)
   (=or
    (=let*
	((_ (=char #\{))
	 (stuff
	   (token-string-until-unbalanced-brace))
	 (_ (=char #\})))
      (result (format nil "${~A}" stuff)))
    (=let*
	((name (string-of (posix-name))))
      (result (format nil "$~A" name)))
    (=let*
	((char (item)))
      (result (format nil "$~C" char))))))


(defun token-meat ()
  "Parser for everything that isn't eof, operator, io-number or newline"
  (=and
      (=not (tracer "NOTOT" (operator-start)))
      (=not (eof-token))
      (=not (=char #\Newline))
      (=not (blank-character))
      (=or
	(backslash-quote)
	(double-quoted-meat)
	(single-quoted-meat)
	(arithmetic-expansion)
	(command-substitution)
	(parameter-expansion)
	(=let* ((item (item))) (result (coerce (list item) 'string))))))

(defun token-string ()
  (=let*
      ((continuation1
	(zero-or-more (=and (=char #\\) (=char #\Newline)
			    (result #.(coerce '(#\\ #\Newline) 'string)))))
       (blanks (string-of (zero-or-more (blank-character))))
       (continuation2
	(zero-or-more (=and (=char #\\) (=char #\Newline)
			    (result #.(coerce '(#\\ #\Newline) 'string)))))
       (token (tracer "TSPT" (posix-token))))
    (result
     (format nil "~{~A~}~A~{~A~}~A"
	     continuation1
	     blanks
	     continuation2
	     (second token)))))

(defun posix-word ()
  (=let* ((meat (one-or-more (token-meat))))
    (result (list :token (apply #'concatenate 'string meat)))))

(defun posix-token ()
  (=and
   (zero-or-more (blank-character))
   (zero-or-more (line-comment))
   (=or
    (operator-token)
    (io-number)
    (tracer "NEWLINE"
    (newline-token))
    (eof-token)
    (posix-word))))

;This needs to be line cached because our
;Tokenizer includes side-effects that disallows backtracking within a line
(defstruct token-source
  (parser (constantly nil) :type function)
  (line-cache nil :type list)
  (input ""))

(defun line-cache-fill (input)
  (when (null (token-source-line-cache input))
    (setf (token-source-line-cache input)
	  (loop for (result) = (funcall (token-source-parser input)
					(token-source-input input))
	     collect (car result)
	     do (setf (token-source-input input) (cdr result))
	     while (and
		    (not (eql (caar result) :eof))
		     (not (eql (caar result) :newline))))))
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
     :input (token-source-input input))))

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
   
(defun <complete-command> ()
  (=or
   (=not (item))
   (=satisfies (lambda (x) (and (consp x) (eql (car x) :eof))))
   (=let* ((_ (<newline-list>)))
     (result
      '(())))
   (=let*
	((list (<list>))
	(op (maybe (<separator>))))
     (let
	 ((fixed-list (append list (list op))))
       (result
	(convert-maybe-async-list fixed-list))))))

(defun collect-list-and-separators (list-sep last)
  (loop for list = list-sep then (cdr list)
       when list
       append (car list)
       else collect last
       while list))

(defun <list> ()
  (=let*
      ((first (<and-or>))
       (rest
	(zero-or-more
	 (=let* ((op (<separator-op>))
		 (andor (<and-or>)))
	   (result (list op andor))))))
    (result (cons first (reduce #'append rest)))))

(defun <and-or> ()
  (=let*
      ((conditionals
	(zero-or-more
	 (=let*
	     ((pipe (<pipeline>))
	      (op
	       (=or (<and-if>)
		    (<or-if>)))
	      (_ (<linebreak>)))
	   (result
	    (list pipe op)))))
       (last (<pipeline>)))
    (result
     (reduce (lambda (x y)
	       (destructuring-bind (pipe op) x
		 (if
		  (eql (car op) :&&)
		       (list 'plush::shell-and pipe y)
		       (list 'plush::shell-or pipe y))))
		  conditionals
	       :from-end t :initial-value last))))

(defun <pipeline> ()
    (=let*
	((bang (maybe (rword :!)))
	 (sequence (<pipe-sequence>)))
      (result
       (if bang
	   `(plush::invert-result ,sequence)
	   sequence))))

(defun <pipe-sequence> ()
  (=let*
      ((stuff
	(zero-or-more
	 (=prog1
	  (<command>)
	  (<vbar>)
	  (<linebreak>))))
       (last (<command>)))
    (result
     `(plush::run-pipe
	     ,@stuff
	     ,last))))

(defun <command> ()
  (=or
   (<simple-command>)
   (=let*
       ((cmd (<compound-command>))
	(redir (maybe (<redirect-list>))))
     (result (list 'plush::compound-command cmd redir)))
   (<function-definition>)))

(defun <compound-command> ()
  (=let*
      ((cmd
	(=or (<brace-group>)
	     (<subshell>)
	     (<for-clause>)
	     (<case-clause>)
	     (<if-clause>)
	     (<while-until-clause>))))
    (result
     cmd)))

(defun <subshell> ()
  (=let*
      ((_ (<open-paren>))
       (list (<compound-list>))
       (_ (<close-paren>)))
    (result
     (list 'plush::with-subshell list))))

(defun <compound-list> ()
  (=let*
      ((_ (maybe (<newline-list>)))
       (stuff
	(zero-or-more
	 (=let*
	     ((item (<and-or>))
	      (sep (<separator>)))
	   (result (list item sep)))))
       (last
	(maybe (<and-or>))))
    (result
     (convert-maybe-async-list
      (if last
	  (collect-list-and-separators stuff last)
	  (mapcan #'copy-list stuff))))))

(defun <for-clause> ()
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
       (do-group (<do-group>)))
    (let
	((wordlist (if in wordlist '("$@"))))
      (result
       `(plush::posix-for ,(second name) (plush::expand-word-list ,`(quote ,wordlist)) ,`(quote ,do-group))))))

(defun <name> ()
  (=let*
   ((name (=satisfies (lambda (x)
			(and (eql :token (car x))
			(smug:run (posix-name) (second x)))))))
    (result (list :name (second name)))))


(defun <wordlist> ()
  (=let* ((words (one-or-more (<word>))))
    (result (mapcar #'second words))))

(defun <word> (&optional disallowed-rsvd)
  (=let*
      ((word (=satisfies (compose (curry #'eql :token) #'car))))
    (if
     (member (second word) disallowed-rsvd :test #'string=)
     (fail)
     (result (list :word (second word))))))

(defun <case-clause> ()
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
		     (<compound-list>)
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
	      (meat (maybe (<compound-list>)))
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

(defun <if-clause> ()
  (=let*
      ((_ (rword :if))
       (if-part (<compound-list>))
       (_ (rword :then))
       (then-part (<compound-list>))
       (else-part (<else-part>))
       (_ (rword :fi)))
    (result `(plush::posix-if ,`(quote ,if-part)
			      ,`(quote ,then-part)
			      ,`(quote ,else-part)))))

(defun <else-part> ()
  (=or
   (=let*
       ((_ (rword :elif))
	(elif-part (<compound-list>))
	(_ (rword :then))
	(then-part (<compound-list>))
	(else-part (<else-part>)))
     (result `((plush::run-pipe
		(plush::posix-if ,`(quote ,elif-part)
			       ,`(quote ,then-part)
			       ,`(quote ,else-part))))))
   (=let*
       ((else
	 (maybe
	  (=and
	   (rword :else)
	   (<compound-list>)))))
     (result 
      else))))

(defun <while-until-clause> ()
  (=let*
      ((type
	(=or
	 (rword :while)
	 (rword :until)))
       (while (<compound-list>))
       (do (<do-group>)))
    (result (list type while do))))

(defun <function-definition> ()
  (=let*
      ((fname (<name>))
       (_ (op "("))
       (_ (op ")"))
       (_ (<linebreak>))
       (body (<compound-command>))
       (redirect (maybe (<redirect-list>))))
    (list :fdef fname body redirect)))

(defun <brace-group> ()
  (=let*
      ((_ (rword :{))
       (cmd (<compound-command>))
       (_ (rword :})))
    (list :brace cmd)))

(defun <do-group> ()
  (=prog2
   (rword :do)
   (<compound-list>)
   (rword :done)))

(defun <simple-command> ()
  (=let*
      ((prefix (<cmd-prefix>))
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

(defun <cmd-prefix> ()
  (zero-or-more
   (=or
    (<io-redirect>)
    (<assignment-word>))))

(defun <assignment-word> ()
  (=let*
      ((word (<word>)))
    (if
     (let ((pos (position #\= (second word))))
     (and pos
	  (not (char= (elt (second word) 0) #\=))
	  (funcall
	   (=and
	    (posix-name)
	    (=not (item))) (subseq (second word) 0 pos))))
     (result
      (list :assignment word))
     (fail))))
      
(defun <cmd-suffix> ()
  (zero-or-more
   (=or
    (<io-redirect>)
    (<word>))))

(defun <redirect-list> ()
  (one-or-more (<io-redirect>)))

(defun <io-redirect> ()
  (=let*
      ((number (maybe (=satisfies (lambda (x) (eql (car x) :io-number)))))
       (to
	(=or
	 (<io-file> (second number))
	 (tracer "IOHERE"
	 (<io-here> (second number))))))
    (result 
     (list :io-redirect to))))

(defun <io-file> (number)
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
       (filename (<word>)))
    (result (list :io-file (car op) (second filename)
		  (when number (parse-integer number))))))

(defun <io-here> (number)
  (=let*
      ((op
	(=or
	 (op "<<")
	 (op "<<-")))
       (here-end (<word>)))
    (result
     `(:io-here ,(car op)
		(,(second here-end) ,(second (third op)))
		,(when number (parse-integer number))))))

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
    
(defun get-another-command (rest)
  (destructuring-bind (&optional result) 
      (funcall (<complete-command>) rest)
    (when result
	(values (car result)
		(cdr result)))))
  
(defun parse-posix-stuff (input)
  (let* ((input (make-token-source :parser (posix-token)
				  :input input))
	(parsed-input (funcall (<complete-command>) input)))
    (if parsed-input
	(destructuring-bind
	      ((result . rest))
	    parsed-input
	  (values result rest))
	(error (make-condition 'posix-parse-failed :format-control "Failed to parse")))))

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

(defun do-arithmetic-expansion() (fail)) ; STUB

(defun do-special-parameter-expansion(&optional dquote)
  (=let*
      ((_ (=char #\$))
       (param (=satisfies
	       (lambda (x)
		 (member (make-keyword x) plush::+special-variables+)))))
    (result 
    (plush::get-parameter (string param) dquote))))

(defun do-numeric-parameter-expansion() (fail)) ; STUB
(defun do-double-quote-expansion() (fail)) ; STUB

(defun do-command-expansion()
  (=let*
      ((string (command-substitution)))
    (let
	((string
	  (subseq string
		  (if (equal (char string 0) #\`) 1 2)
		  (1- (length string)))))
    (with-temp-fd  (fd fname)
      (plush::with-subshell (t)
	(isys:dup2 fd iolib/os:+stdout+)
	(mapc #'eval (parse-posix-stuff string))
	(isys:exit 0))
      (result
       (coerce
	(with-open-file (s fname)
	  (loop for c = (read-char s nil :eof)
	       until (eql c :eof)
	       collect c))
	'string))))))

(defun parameter-default ()
  (=let*
      ((_ (=char #\:))
       (_ (=char #\-))
       (word (token-string-until-unbalanced-brace)))
    (result (list :default word))))

(defun parameter-assign ()
  (=let*
      ((_ (=char #\:))
       (_ (=char #\=))
       (word (token-string-until-unbalanced-brace)))
    (result (list :assign word))))

(defun parameter-alternative ()
  (=let*
      ((_ (=char #\:))
       (_ (=char #\+))
       (word (token-string-until-unbalanced-brace)))
    (result (list :alternative word))))

(defun parameter-error ()
  (=let*
      ((_ (=char #\:))
       (_ (=char #\=))
       (word (token-string-until-unbalanced-brace)))
    (result (list :error word))))

(defun parameter-option ()
    (=or
     (parameter-default)
     (parameter-assign)
     (parameter-error)
     (parameter-alternative)))

(defun do-parameter-brace-expansion()
  (=let*
      ((_ (=char #\$))
       (_ (=char #\{))
       (name
	(string-of 
	 (zero-or-more
	  (=satisfies
	   (lambda (x) (and
			(char/= x #\:)
			(char/= x #\})))))))
       (option (maybe (parameter-option)))
       (_ (=char #\})))
    (result
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

(defun do-quote-expansion()
  (single-quoted-meat))

(defun do-backslash-expansion()
  (=let*
      ((_ (=char #\\))
       (c (item)))
    (result (format nil "\\~C" c))))

(defun tilde-expand (is-assignment)
  (=let*
      ((_ (=char #\~))
       (expandme
	(string-of
	 (zero-or-more
	  (=satisfies
	   (lambda (x)
	     (not
	      (or
	      (char= x #\/)
	      (and is-assignment (char= x #\:))))))))))
     (if (string= expandme "")
	 (result (iolib/os:environment-variable "HOME"))
	 (result (nth-value 5 (isys:getpwnam expandme))))))

(defun colon-tilde-expand ()
  (=let*
      ((_ (=char #\:))
       (expansion (tilde-expand t)))
    (result (format nil ":~A" expansion))))

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

(defun do-name-parameter-expansion ()
  (=let* ((_ (=char #\$))
	  (name
	   (string-of
	    (posix-name))))
    (let ((v (plush::get-parameter name)))
      (result (or v "")))))


;TODO apply field splitting to results of expansion
(defun expand-word-parser (is-assignment)
  (=let*
      ((start
	(maybe
	 (tilde-expand is-assignment)))
       (rest
	(zero-or-more
	 (=or
	  (do-arithmetic-expansion)
	  (do-command-expansion)
	  (tracer "PBE"
	  (do-parameter-brace-expansion))
	  (do-special-parameter-expansion)
	  (do-numeric-parameter-expansion)
	  (do-name-parameter-expansion)
	  (do-double-quote-expansion)
	  (do-quote-expansion)
	  (do-backslash-expansion)
	  (if is-assignment
	    (colon-tilde-expand)
	    (fail))
	  (item)))))
    (result (join-strings-preserving-lists
	     (if start
		 (cons start rest)
		 rest)))))

(defun glob-equiv () (fail)) ;TODO STUB
(defun glob-class () (fail)) ;TODO STUB
(defun glob-collating () (fail)) ;TODO STUB

(defun glob-bracket ()
  (=let*
      ((invert (maybe (=char #\!)))
       (stuff
	(zero-or-more
	 (=or
	  (glob-collating)
	  (glob-equiv)
	  (glob-class)
	  (glob-quote)
	  (=satisfies (lambda (x) (char/= x #\]))))))
       (_ (=char #\])))
    (if stuff
	(result `(,(if invert
		       :inverted-char-class
		       :char-class) ,@(flatten stuff)))
	"[]")))

(defun glob-special ()
  (=let*
      ((special-char
	(=or
	 (=char #\*)
	 (=char #\?)
	 (=char #\[))))
    (cond
      ((char= special-char #\*)
       (result '(:greedy-repetition 0 nil :everything)))
      ((char= special-char #\?)
       (result :everything))
      ((char= special-char #\[)
       (glob-bracket)))))

(defun glob-quote-as-sequence ()
  (=let* ((stuff (glob-quote)))
    (result `(:sequence ,@(or stuff '(:void))))))

(defun glob-quote ()
  (=or
   (=let* ((_ (=char #\\)))
     (exactly 1 (item)))
   (=let* ((_ (=char #\'))
	 (stuff (zero-or-more (=satisfies (lambda (x) (char/= x #\')))))
	 (_ (=char #\')))
     (result stuff)
   (=let* ((_ (=char #\"))
	 (stuff
	  (zero-or-more
	   (=or
	    (=let* ((_ (=char #\\))
		    (x (item)))
	      (result x))
	    (=satisfies (lambda (x) (char/= x #\"))))))
	 (_ (=char #\")))
     (result stuff)))))

(defun glob-parser ()
  (=let*
      ((stuff
	(zero-or-more
	 (=or
	  (glob-special)
	  (glob-quote-as-sequence)
	  (item)))))
    (result `(:sequence :start-anchor ,@stuff :end-anchor))))

(defun unary-op ()
  (=let*
      ((op (string-of
	    (=or
	     (=char #\+)
	     (=char #\-)
	     (=char #\~)
	     (=char #\!)))))
    (result (make-keyword op))))

(defun unary-and-primary-expr ()
  (=let*
      ((unary (maybe (unary-op)))
       (primary (primary-expr)))
    (result
     (if unary (list unary primary) primary))))

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

(defun primary-expr ()
  (=or
   (=let*
       ((_ (=char #\())
	(expr (arithmetic-expr2))
	(_ (=char #\))))
     (result expr))
   (=let*
       ((name (posix-name)))
     (result (list :var name)))
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
	    (parse-integer number :radix base))))))
