;;;; plush.lssp

(in-package #:plush)
(declaim (optimize (debug 3)))

;TODO move to utility package
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

(defmacro continuable-block (&body b)
  (with-gensyms (c)
    `(let ((,c (catch 'continue ,@b 0)))
       (when (> 0 ,c) (throw 'continue (1- ,c))))))

(defmacro breakable-block (&body b)
  (with-gensyms (c)
    `(let ((,c (catch 'break ,@b 0)))
       (when (> 0 ,c) (throw 'break (1- ,c))))))

(defparameter +default-ifs+ #.(coerce '(#\Space #\Tab #\Newline) 'string))

(defun lexecv (path argv &key (search-path nil))
  (cffi:with-foreign-object (cargv :pointer (1+ (length argv)))
    (isys:bzero cargv (* (1+ (length argv)) (isys:sizeof :pointer)))
    (unwind-protect
	 (progn
	   (iolib/os::allocate-argv cargv (car argv) (cdr argv))
	   (if search-path
	       (isys:execvp path cargv)
	       (isys:execv path cargv)))
      (iolib/os::delocate-null-ended-list cargv))))

; TODO Implement special utilites
(defparameter +shell-special-utilities+ '(
					  ("break" . make-special-utility)
					  (":" . make-special-utility)
					  ("continue" . make-special-utility)
					  ("." . make-special-utility)
					  ("eval" . make-special-utility)
					  ("exec" . make-special-utility)
					  ("export" . make-special-utility)
					  ("unset" . make-special-utility)
					  ("return" . make-special-utility)
					  ("exit" . make-special-utility)
					  ))
; TODO implement some utilities
(defparameter +shell-utilities+ '(
				  ("alias" . make-utility)
				  ;bg
				  ("cd" . make-utility)
				  ;command
				  ;false
				  ;fc
				  ;fg
				  ;getopts
				  ("jobs" . make-utility)
				  ;kill
				  ;newgrp
				  ;pwd
				  ;read
				  ;true
				  ("umask" . make-utility)
				  ;unalias
				  ;wait
				  ("[" . make-utility)))

(defparameter +special-variables+ '(:@ :# :* :? :- :$ :! :0))


(defstruct (shell-environment (:conc-name :se-))
  (flags () :type list)
  (exported-environment-vars
   (hash-table-keys (iolib/os::environment-variables (iolib/os:environment)))
   :type list)
  (numeric-arguments () :type list)
  (arg0 "" :type string)
  (toplevel-pid (isys:getpid) :type fixnum)
  (functions (make-hash-table :test #'equal) :type hash-table)
  (aliases (make-hash-table :test #'equal) :type hash-table)
  (last-returnval 0 :type fixnum)
  (last-bg-pid nil :type (or null fixnum))
  (bg-jobs (make-hash-table) :type hash-table))
  
(defparameter *current-shell-environment* (make-shell-environment))

(defclass command ()
  ((words :initarg :words
	  :initform nil
	  :accessor command-words
	  :type list)
   (assignments :initarg :assignments
		:initform nil
		:accessor command-assignments
		:type list)
   (redirects :initarg :redirects
	      :initform nil
	      :accessor command-redirects
	      :type list)))
    
(defclass utility (command) ())

(defun make-utility (words assignments redirects)
    (make-instance 'utility
		   :words words
		   :assignments assignments
		   :redirects redirects))

(defclass special-utility (command) ())

(defun make-special-utility (words assignments redirects)
  (make-instance 'special-utility
		 :words words
		 :assignments assignments
		 :redirects redirects))

(defclass program-command (command)
  ((path :initarg :path
	 :accessor program-command-path
	 :initform ""
	 :type string)))

(defun make-program-command (path argv assignments redirects)
  (make-instance 'program-command
		 :path path
		 :words argv
		 :assignments assignments
		 :redirects redirects))

(defclass function-command (command)
 ((fn :initarg :fn)
  (internal-redirects :initarg :internal-redirects)))

(defun make-function-command (fn words assignments redirects)
  (make-instance 'function-command
		 :fn (car fn)
		 :internal-redirects (cdr fn)
		 :redirects redirects
		 :words words
		 :assignments assignments))

(defgeneric run-command (command &key subshell))

(defmacro with-redirected-io ((redirects) &body b)
  (with-gensyms (io-save)
    `(let
	 ((,io-save (do-redirects ,redirects t)))
       (unwind-protect
	    (progn
	      ,@b)
	      (undo-redirects ,io-save)))))

(defun jobs-command (ut)
  ;TODO add -l and -p support
  (loop for item being each hash-key in (se-bg-jobs *current-shell-environment*)
       do (format t "~A~%" item))
  0)

;TODO assignments and redirects
(defmethod run-command ((ut utility) &key &allow-other-keys)
  (let ((env (iolib/os:environment)))
    (set-command-environment)
    (setf (iolib/os:environment)
	  (assignments-to-env (command-assignments ut)))
    (unwind-protect
	 (case (make-keyword (car (command-words ut)))
	   (:|alias|
	     (alias-command ut))
	   (:|cd|
             (let
               ((abspath
                  (iolib/os:absolute-file-path
                    (or
                      (second (command-words ut))
                      (get-parameter "HOME"))
                    (iolib/os:current-directory))))
                (handler-case
                  (progn
                    (setf (iolib/os:current-directory) abspath)
                    (setf iolib/pathnames:*default-file-path-defaults* abspath)
                    0)
                  (iolib/syscalls:enoent (v) (declare (ignore v)) 127))))
	   (:|jobs|
	     (jobs-command ut))
	   (:|umask|
	     (umask-command ut))
	   (:|[|
	     (run-command (make-program-command "/usr/bin/[" (command-words ut)
						(command-assignments ut)
						(command-redirects ut)))))
      (setf (iolib/os:environment) env))))

(defun return-command (ut)
  (when (second (command-words ut))
    (setf (se-last-returnval *current-shell-environment*)
	  (or (parse-integer (Second (command-words ut)) :junk-allowed t)
	      255))
    (throw 'plush-return (se-last-returnval *current-shell-environment*))))
				 
(defun dot-command (ut)
  (let* ((script (second (command-words ut)))
	 (script (if (position #\/ script)
		     script
		     (search-path script))))
    (format *error-output* ". ~a~%" script)
    (catch 'plush-return
      (with-open-file (f script)
	(posix-repl :input-stream f :prompt nil :debug t))
      (format *error-output* "DONE: ~a~%" script))
    (se-last-returnval *current-shell-environment*)))

(defun quote-string (string)
  (with-output-to-string (outs)
    (write-char #\' outs)
    (loop for char across string
	 when (char= char #\')
	 do (write-string "'\''" outs)
	 else do (write-char char outs))
    (write-char #\' outs)))

(defun one-alias (alias)
  (let ((pos (find-first-unquoted alias (lambda (x) (char= x #\=)))))
    (if pos
	(setf (gethash (subseq alias 0 pos) (se-aliases *current-shell-environment*))
	      (subseq alias (1+ pos)))
	(format t "alias ~a=~a~%" alias (quote-string
					 (gethash alias (se-aliases *current-shell-environment*)))))))

(defun alias-command (ut)
  (if (cdr (command-words ut))
      (loop for alias in (cdr (command-words ut))
	   do (one-alias alias))
      (loop for k being the hash-keys of (se-aliases *current-shell-environment*)
	   using (hash-value v)
	   do (format t "alias ~a=~a~%" k (quote-string v))))
  0)

(defun alias-substitute (word)
  (gethash word (se-aliases *current-shell-environment*)))

(defun unset-command (ut)
  (loop for var in (cdr (command-words ut))
       do (iolib/os:makunbound-environment-variable var))
  0)

(defun symbolic-umask ()
  (let ((umask (isys:umask 0)))
    (isys:umask umask)
    (apply #'format nil "u=~a,g=~a,o=~a~%"
	   (loop for i from 6 downto 0 by 3
	      collect (format nil "~[~;x~;w~;wx~;r~;rx~;rw~;rwx~]"
			      (ldb (byte 3 i) (lognot umask)))))))

(defun umask-command (ut)
  (cond
    ((not (cdr (command-words ut)))
     (let
	 ((realumask (isys:umask 0)))
       (isys:umask realumask)
       (cffi:with-foreign-string ((str len) (format nil "0~3,'0O~%" realumask))
	 (isys:write 0 str len))))
    ((and
      (string= (cadr (command-words ut)) "-S")
      (not (cddr (command-words ut))))
     (cffi:with-foreign-string ((str len) (symbolic-umask))
       (isys:write 0 str len)))
    (t
     (let ((mask
	    (if (string= (cadr (command-words ut)) "-S")
		(caddr (command-words ut))
		(cadr (command-words ut)))))
       (if (char= (char mask 0) #\0)
	   (isys:umask (parse-integer mask :radix 8))
	   (with-temp-fd (fd name)
	     (let ((umask (isys:umask 0)))
	       (isys:umask umask)
	       (isys:fchmod fd (logand #o777 (lognot umask))))
	     (run-command (make-program-command "/bin/chmod" `("chmod" ,mask ,name) nil nil))
	     (isys:umask (logand #o777 (lognot (isys:stat-mode (isys:stat name))))))))
     (when (string= (cadr (command-words ut)) "-S")
     (cffi:with-foreign-string ((str len) (symbolic-umask))
       (isys:write 0 str len)))))
  0)
	    

(defun export-command (ut)
  (if (string= (second (command-words ut)) "-p")
      (loop for item in (se-exported-environment-vars *current-shell-environment*)
	 for v = (iolib/os:environment-variable item)
					;TODO output to stdout instead of *standard-output*
	 when v do (format t "export ~a=~a~%" item v)
	 else do (format t "export ~A~%" item)
	 finally (return 0))
      (let* ((vars (cdr (command-words ut))))
	(loop for var in vars
	   for pos = (position #\= var)
	   for vname = (subseq var 0 pos)
	   for vval = (and pos (subseq var (1+ pos)))
	   do (pushnew vname (se-exported-environment-vars *current-shell-environment*)
		       :test #'string=)
	   when vval do (setf (iolib/os:environment-variable vname) vval))
	(se-last-returnval *current-shell-environment*))))

(defun throw-command (ut tag)
  (cond
    ((= (length (command-words ut)) 1)
     (throw tag 0))
    ((and
      (= (length (command-words ut)) 2)
      (parse-integer (second (command-words ut))))
     (throw tag (1- (parse-integer (Second (command-words ut))))))))

(defun eval-command (ut)
  (if (some (curry #'string/= "") (cdr (command-words ut)))
      (progn
	(mapc #'eval
	      (plush-parser::parse-posix-stuff (format nil "~{~A~^ ~}" (cdr (command-words ut)))))
	(se-last-returnval *current-shell-environment*))
      0))

(defun exit-command (ut)
  (let ((ev (and (second (command-words ut)) (parse-integer (second (command-words ut))))))
    (if ev
	(isys:exit ev)
	(isys:exit (se-last-returnval *current-shell-environment*)))))

(defun exec-command (ut)
  (handler-case
      (prog1
	  0
	(lexecv (cadr (command-words ut)) (cdr (command-words ut)) :search-path t))
    (iolib/syscalls:enoent () 127)))

(defmethod run-command ((ut special-utility) &key &allow-other-keys)
  (setf (iolib/os:environment)
	(assignments-to-env (command-assignments ut)))
  (setf (se-last-returnval *current-shell-environment*) 0)
  ;POSIX states that variable assignments that are part of
  ;a special-utility command will persist.
  ;bash disagrees (e.g. : FOO=BAR), except when invoked as "sh"
  (let ((cmd-name (make-keyword (car (command-words ut)))))
    (cond
      ((and (eql cmd-name :|exec|)
	    (= (length (command-words ut)) 1))
       (do-redirects (command-redirects ut))
       0)
      (t
       (with-redirected-io ((command-redirects ut))
	 (case cmd-name
	   (:|break|
	     (throw-command ut 'break))
	   (:|:|
	     0)
	   (:|continue|
	     (throw-command ut 'continue))
	   (:|.|
	     (dot-command ut))
	   (:|eval|
	     (eval-command ut))
	   (:|exec|
	     (exec-command ut))
	   (:|exit|
	     (exit-command ut))
	   (:|export|
	     (export-command ut))
	   (:|return|
	     (return-command ut))
	   (:|unset|
	     (unset-command ut))
	   ))))))

(defun assignments-to-env (assignments)
  (let ((env (iolib/os:environment)))
    (mapc (lambda (asn)
	    (let* ((eqpos (position #\= asn)))
	      (setf (iolib/os:environment-variable
		     (subseq asn 0 eqpos) env)
		    (subseq asn (1+ eqpos)))))
	  assignments)
    env))

(defun set-vars (assignments)
  (setf (iolib/os:environment)
	(assignments-to-env assignments)))

(defun fd-from-op (op)
  (case op
    ((:> :>> :>& :>\|) 1)
    ((:< :<< :<<- :<> :<&) 0)))

(defun flags-from-op (op)
  (case op
    (:> (logior isys:o-wronly isys:o-trunc isys:o-creat))
    (:>\| (logior isys:o-wronly isys:o-trunc isys:o-creat))
    (:>> (logior isys:o-wronly isys:o-append isys:o-creat))
    ((:< :<< :<<-) (logior isys:o-rdonly))
    (:<> (logior isys:o-rdwr isys:o-creat))))


;TODO stub
(defun remove-tabs (stuff)
  (warn "STUB: remove-tabs")
  stuff)

;TODO stub
;TODO respect noclobber flag
(defun do-redirects (redirects &optional save)
  (loop for (type op stuff fd-raw) in redirects
     for fd = (or fd-raw (fd-from-op op))
       for expanded-stuff =(unquote (car (first-expand-word stuff nil t)))
     ;when (null fd) do (setf fd (fd-from-op op))
     ;do (format t "~&FD-IN_REDIRECT: ~S ~S ~S~%" fd-raw fd op)
     when save
     collect (isys:dup fd)
     and collect fd
     end
     when (eq type :io-file)
     do 
       (if (member op '(:<& :>&))
	   (if (string= expanded-stuff "-")
	       (handler-case (isys:close fd)
		 (isys:ebadf () (values)))
	       (isys:dup2 (parse-integer stuff) fd))
	   (let* (
		  (newfd (isys:open expanded-stuff (flags-from-op op))))
	     (isys:dup2 newfd fd)
	     (isys:close newfd)))
     else
     do (with-temp-fd (newfd fname)
	  (isys:dup2 newfd fd)
	  (let*
	      ((flags (isys:fcntl fd isys:f-getfl)))
	    (logior
	     (logand flags (lognot isys:o-rdwr))
	     isys:o-rdonly))
	  (let*
	      ((meat (eval (second stuff)))
	       (meat
		(if (eql op :<<-)
		    (remove-tabs meat)
		    meat)))
	    (cffi:with-foreign-string ((str len) meat)
	      (isys:write newfd str (1- len)))
	    (isys:lseek newfd 0 isys:seek-set)))))


(defun undo-redirects (saved)
  (loop
     for from = (and saved (pop saved))
     for to = (and saved (pop saved))
     while from
     do (isys:dup2 from to)
     do (isys:close from)))
     
;TODO copy shell environment properly
(defmethod run-command ((command program-command) &key subshell)
  (let ((pid (if subshell 0 (isys:fork))))
    (when (= pid 0)
      (set-command-environment)
      (progn
	(do-redirects (command-redirects command))
	(setf (iolib/os:environment)
	      (assignments-to-env (command-assignments command)))
        (handler-case
          (lexecv (program-command-path command) (command-words command))
          (iolib/syscalls:enoent () (isys:exit 127)))))
    (let ((status (nth-value 1 (isys:waitpid pid 0))))
      (if
        (isys:wifexited status)
        (isys:wexitstatus status)
        0))))

(defmethod run-command ((command function) &key subshell)
  (funcall command :subshell subshell))

(defmethod run-command ((command function-command) &key &allow-other-keys)
  (with-redirected-io ((command-redirects command))
    (with-redirected-io ((slot-value command 'internal-redirects))
     (let ((env-save (iolib/os:environment)))
       (unwind-protect
	    (progn
	      (setf (iolib/os:environment)
		    (assignments-to-env (command-assignments command)))
	      (catch 'plush-return
		(run-pipe (eval (slot-value command 'fn))))
	      (se-last-returnval *current-shell-environment*))
	 (setf (iolib/os:environment) env-save))))))



(defun wait-for-job (pid) ;TODO stub
  (warn "STUB: wait-for-job")
  (isys:waitpid pid 0))

(defvar *path-cache* (make-hash-table :test #'equal))

(defun search-path (cmd)
  (if
    (and (gethash cmd *path-cache*)
         (iolib/os:file-kind (gethash cmd *path-cache*)))
    (gethash cmd *path-cache*)
    (loop for path in
	 (split-sequence #\: (iolib/os:environment-variable "PATH"))
	 for cmd-path = (iolib/pathnames:merge-file-paths cmd path)
	 when (iolib/os:file-exists-p cmd-path) ;TODO check for permissions
         do (setf (gethash cmd *path-cache*) cmd-path)
	 and return cmd-path)))

(defun set-command-environment ()
  (let ((oldenv (iolib/os:environment)))
    (iolib/os:clear-environment)
    (loop for item in (se-exported-environment-vars *current-shell-environment*)
       for v = (iolib/os:environment-variable item oldenv)
       when v do (setf (iolib/os:environment-variable item) v))))

(defun simple-command (words redirects assignments)
  (if
   (position #\/ (car words))
     (make-program-command (car words) words assignments redirects)
  (cond
    ((null words)
     (lambda (&key &allow-other-keys)
       (set-vars assignments)
       0))
    
    ((assoc (car words) +shell-special-utilities+ :test #'equal)
     (funcall (cdr (assoc (car words) +shell-special-utilities+ :test #'equal))
	      words assignments redirects))
    ((gethash (car words) (se-functions *current-shell-environment*))
     (make-function-command
      (gethash (car words) (se-functions *current-shell-environment*))
      words assignments redirects))
    ((assoc (car words) +shell-utilities+ :test #'equal)
     (funcall (cdr (assoc (car words) +shell-utilities+ :test #'equal))
	      words assignments redirects))
    ((let ((cmd (search-path (car words))))
       (when cmd
	    (make-program-command (iolib/pathnames:file-path-namestring cmd)
				  words assignments redirects))))
    (t
     (format *error-output* "Unable to locate ~A~%(Path: %~A)"
	     (car words)
	     (iolib/os:environment-variable "PATH"))
     (lambda (&key &allow-other-keys)
       127)))))

;TODO stub
(defun init-subshell ()
  (warn "STUB: init-subshell"))

(defmacro with-subshell ((&optional wait) &body b)
  `(let ((pid (isys:fork)))
     (cond
       ((= pid 0)
	(init-subshell)
	(progn ,@b)
	(isys:exit (se-last-returnval *current-shell-environment*)))
       (t
	(if ,wait
	  (nth-value 1 (isys:waitpid pid 0))
	  pid)))))

(defmacro asynchronous-cmd (&body b)
  (with-gensyms (job-id pid)
    `(let ((,job-id
	    (loop for i from 1
	       when (not (gethash i (se-bg-jobs *current-shell-environment*) nil))
	       return i))
	   (,pid (with-subshell ()
		   (isys:setpgid 0 0)
		   ,@b)))
       (setf (se-last-bg-pid *current-shell-environment*) ,pid
	     (gethash ,job-id (se-bg-jobs *current-shell-environment*)) ,pid))))

(defun reap-children (se)
  (loop for (job-id . pid) in (hash-table-alist (se-bg-jobs se))
     for stat = (and pid (isys:waitpid pid isys:wnohang))
     if stat
     do
       (cond
	 ((isys:wifexited stat)
	  (format t "[~D]+  Done~%" job-id)
	  (remhash job-id (se-bg-jobs se)))
	 ((isys:wifsignaled stat)
	  (format t "[~D]+  Terminated~%" job-id)
	  (remhash job-id (se-bg-jobs se))))))

(defun run-with-pipe (cmd stdin stdout subshell)
  (unwind-protect
       (if subshell
	   (with-subshell ()
	     (when stdin
	       (isys:dup2 stdin iolib/os:+stdin+)
	       (isys:close stdin))
	     (when stdout
	       (isys:dup2 stdout iolib/os:+stdout+)
	       (isys:close stdout))
	   (run-command cmd :subshell t))
	   (let ((oldstdin (when stdin (isys:dup iolib/os:+stdin+)))
		 (oldstdout (when stdout (isys:dup iolib/os:+stdout+))))
	     (when stdin
	       (isys:dup2 stdin iolib/os:+stdin+))
	     (when stdout
	       (isys:dup2 stdout iolib/os:+stdout+))
	     (unwind-protect
		  (run-command cmd)
	       (when oldstdin
		 (isys:dup2 oldstdin iolib/os:+stdin+)
		 (isys:close oldstdin))
	       (when oldstdout
		 (isys:dup2 oldstdout iolib/os:+stdout+)
		 (isys:close oldstdout)))))
    (when stdin (isys:close stdin))
    (when stdout (isys:close stdout))))
		

(defun run-pipe (&rest commands)
  (loop
       with pipeout = nil
       while commands
     for cmd = (pop commands)
     when commands
     do
       (multiple-value-bind
	     (read-end write-end) (isys:pipe)
	 (run-with-pipe cmd pipeout write-end t)
	 (setf pipeout read-end))
     else do (setf (se-last-returnval *current-shell-environment*)
		   (run-with-pipe cmd pipeout nil nil))))

(defmacro shell-and (a b)
  `(progn
     ,a
     (when (= 0 (se-last-returnval *current-shell-environment*))
       ,b)))

(defmacro shell-or (a b)
  `(progn
     ,a
     (when (/= 0 (se-last-returnval *current-shell-environment*))
       ,b)))

(defun do-unquote (char state)
  (case state
    (:outer
     (cond
       ((char= char #\") (list nil :double-quote))
       ((char= char #\\) (list nil :backslash))
       ((char= char #\') (list nil :single-quote))
       (t (list char :outer))))
    (:double-quote
     (cond
       ((char= char #\") (list nil :outer))
       ((char= char #\\) (list nil :dq-backslash))
       (t (list char :double-quote))))
    (:backslash
     (list char :outer))
    (:dq-backslash
     (list char :double-quote))
    (:single-quote
     (cond
       ((char= char #\') (list nil :outer))
       (t (list char :single-quote))))))

(defun unquote (word)
  (coerce
   (loop for char across word
      with state = :outer
      for (collectme next-state) = (do-unquote char state)
      when collectme collect collectme
      do (setf state next-state)) 'string))

(defun find-first-unquoted (word pred)
  (loop for char across word
     with state = :outer
     for (collectme next-state) = (do-unquote char state)
     for position = 0 then (1+ position)
     when (and (funcall pred char)
	       (eql state :outer))
     return position
     do (setf state next-state)
     finally (return nil)))

(defun first-expand-word (word vas split)
  (declare (ignorable split))
  (esrap::parse (if vas
		    'plush-token::expand-word-parser-assignment
		    'plush-token::expand-word-parser) word))
  ;(smug:run (plush-parser::expand-word-parser vas) word))

(defun expand-here-doc (doc)
  (apply 'concatenate 'string
  (esrap:parse 'plush-parser::expand-here-doc-parser doc)))

(defun first-expansions (list vas split)
  (reduce #'append
	  (mapcar (lambda (x)
		    (first-expand-word x vas split)) list)))

(defun split-quoted-path (word)
  (let ((parts (split-sequence #\/ word)))
    (loop for word in parts
       for nudged-word = word then
	 (case fixed-state
	   ((:outer :backslash) word)
	   ((:double-quote :dq-backslash)
	    (concatenate 'string "\"" word))
	   (:single-quote (concatenate 'string "'" word)))
       for fixed-state =
	 (loop for char across nudged-word
	    with state = :outer
	    for (collectme next-state) = (do-unquote char state)
	    do (setf state next-state)
	    finally (return state))
       collect
	 (case fixed-state
	   (:outer nudged-word)
	   (:backslash (subseq nudged-word 0 (1- (length nudged-word))))
	   ((:double-quote) (concatenate 'string nudged-word "\""))
	   (:dq-backslash (concatenate 'string
				       (subseq nudged-word 0 (1- (length word)))
				       "\""))
	   (:single-quote (concatenate 'string nudged-word "'"))))))


(defun glob-to-pcre (glob)
  (esrap::parse 'plush-token::glob-parser glob))
  ;(caar (funcall (plush-parser::glob-parser) glob)))

(defun match-one-glob (directory glob)
  (let* ((starts-with-dot
	  (and (> (length (unquote glob)) 0)
	  (char= (char (unquote glob) 0) #\.)))
	(scanner 
	 (cl-ppcre:create-scanner
	  (if starts-with-dot
	      (glob-to-pcre glob)
	      `(:sequence
		(:negative-lookahead #\.)
		,(glob-to-pcre glob)))
	      :single-line-mode t)))
    (loop
       for path in
	 (let ((list
		(ignore-errors 
		  (iolib/os:list-directory directory))))
	   (when list (append '("." "..") list)))
       when (cl-ppcre:scan scanner (iolib/pathnames:file-path-namestring path))
       collect
	 (iolib/pathnames:file-path-file path))))

(defun walk-path-expansion (parts &optional directory)
  (cond
    ((not directory)
     (if (eql (car parts) :root)
	 (mapcar (lambda (x)
		   (format nil "/~A" x))
		 (walk-path-expansion (cdr parts) "/"))
	 (walk-path-expansion parts (isys:getcwd))))
    ((not (cdr parts))
     (match-one-glob directory (car parts)))
    (t (mapcan
	(lambda (subdir)
	  (let ((full-path
		 (iolib/pathnames:merge-file-paths
		  subdir directory)))
	    (when
		(iolib/os:directory-exists-p full-path)
	      (mapcar
	       (lambda (child)
		 (format nil "~A/~A" subdir child))
	       (walk-path-expansion (cdr parts) full-path)))))
	(match-one-glob directory (car parts))))))

(defun expand-path (word)
  (if (not
       (member-if-not (lambda (x)
			(or (characterp x)
			    (eql x :sequence)
			    (eql x :start-anchor)
			    (eql x :end-anchor)))
		  (flatten (glob-to-pcre word))))
      (list word)
      (let* ((parts (split-quoted-path word))
	     (parts (if
		     (and (> (length parts) 1)
			  (string= (plush::unquote (car parts)) ""))
		     (cons :root (cdr parts))
		     parts))
	     (matches (walk-path-expansion parts)))
	(or matches (list word)))))

(defun path-expansion (list)
  (if (member #\f (se-flags *current-shell-environment*))
      list
      (reduce #'append
	      (mapcar #'expand-path list))))

(defun expand-word-list (list &key variable-assignment
				(split-fields t))
  (let*
      ((list (first-expansions list variable-assignment split-fields))
       (list (path-expansion list))
       (list (mapcar #'unquote list)))
    list))


(defun run-script (string)
  (mapc #'eval (plush-parser::parse-posix-stuff string))
  (values))

(defun expand-args ()
  (let* ((ifs (or (get-parameter
		   "IFS")
		  +default-ifs+))
	 (separator
	  (if (> (length ifs) 0) (char ifs 0) " ")))
    (format nil "~{~A~^~C~}"
	    (mapcan (lambda (x)
		      (list
		       separator x))
		    (se-numeric-arguments *current-shell-environment*)))))

(defun get-parameter (name &optional dquote)
  (case (make-keyword name)
    (:@
     (if dquote
	 (mapcar (curry #'format nil "\"~A\"")
		 (se-numeric-arguments *current-shell-environment*))
	 (expand-args)
	 ))
    (:* (expand-args))
    (:# (format nil "~D" (length (se-numeric-arguments *current-shell-environment*))))
    (:? (format nil "~D" (se-last-returnval *current-shell-environment*)))
    (:- (format nil "~{~A~}" (se-flags *current-shell-environment*)))
    (:$ (format nil "~D" (se-toplevel-pid *current-shell-environment*)))
    (:! (format nil "~D" (se-last-bg-pid *current-shell-environment*)))
    (:0 (se-arg0 *current-shell-environment*))
    (t
     (multiple-value-bind (number end)
	 (parse-integer name :junk-allowed t)
       (if (= end (length name))
	   (nth (1- number) (se-numeric-arguments *current-shell-environment*))
	   (iolib/os:environment-variable name))))))

(defun read-and-parse-posix-stuff (&key (input t) debug linecount)
  (loop
       for lineno from 1
       for line = (read-line input nil :eof)
     for sofar = (and (not (eql line :eof)) (format nil "~A~%" line))
       then (concatenate 'string sofar (format nil "~A~%" line))
       when debug do (format *error-output* "~D~%" (+ lineno linecount))
       when (eql line :eof) return :eof
       do (let
	      ((it (and
		    (not (string= line ""))
		    (not (char= (char sofar (- (length sofar) 2)) #\\))
		    (handler-case (plush-parser::parse-posix-stuff sofar)
		      (plush-token::eof-when-tokenizing nil)
			  (plush-parser::posix-parse-failed nil)))))
	    (when it (return (values it (+ lineno linecount)))))))

(defun posix-repl (&key (input-stream t) debug &allow-other-keys)
  (loop
     for lines = 0 then lines2
     for ( expr lines2 ) = (multiple-value-list (read-and-parse-posix-stuff :input input-stream :debug debug :linecount lines))
       when debug do (format *error-output* "~A~%" expr)
     until (eql expr :eof)
     do (restart-case
	    (mapc #'eval expr)
	  (plush-continue nil))))

(defun compound-command (cmd redirects)
  (setf (command-redirects cmd) redirects)
  cmd)

(defclass if-command (command)
  ((test :initform nil
	 :initarg :test
	 :accessor if-command-test)
  (then :initform nil
	 :initarg :then
	 :accessor if-command-then)
  (else :initform nil
	 :initarg :else
	 :accessor if-command-else)))

(defun posix-if (test then else)
  (make-instance 'if-command
		  :test test
		  :then then
		  :else else))
	      
(defun myeval (x)
  (eval x))

(defmethod run-command ((cmd if-command) &key &allow-other-keys)
  (with-redirected-io ((command-redirects cmd))
    (mapc #'myeval (if-command-test cmd))
    (if (= 0 (se-last-returnval *current-shell-environment*))
	(mapc #'myeval (if-command-then cmd))
	(mapc #'myeval (if-command-else cmd))))
  (se-last-returnval *current-shell-environment*))


(defclass brace-group (command)
  ((commands :initform nil :initarg :commands :accessor brace-commands)))



(defmethod run-command ((cmd brace-group) &key &allow-other-keys)
  (with-redirected-io ((command-redirects cmd))
    (mapc (lambda (x) (when x) (eval x))
	    (brace-commands cmd))
    (se-last-returnval *current-shell-environment*)))

(defun brace-group (commands)
  (make-instance 'brace-group
		 :commands commands))

(defclass subshell-group (command)
  ((commands :initform nil :initarg :commands :accessor subshell-commands)))

(defmethod run-command ((cmd subshell-group) &key &allow-other-keys)
  (with-redirected-io ((command-redirects cmd))
    (with-subshell (t)
	(mapc (lambda (x) (when x) (eval x))
	      (subshell-commands cmd))
      (se-last-returnval *current-shell-environment*))))

(defun subshell-group (commands)
  (make-instance 'subshell-group
		 :commands commands))

(defclass for-command (command)
  ((name :initform nil
	 :initarg :name
	 :accessor for-command-name)
  (words :initform nil
	 :initarg :words
	 :accessor for-command-words)
  (commands :initform nil
	 :initarg :commands
	 :accessor for-command-commands)))

(defun posix-for (name words commands)
  (make-instance 'for-command
		 :name name
		 :words words
		 :commands commands))

(defclass while-until-command (command)
  ((until :initform nil :initarg :until
	  :accessor is-until)
   (test :initform nil :initarg :test
	 :accessor loop-test)
   (commands :initform nil
	    :initarg :commands
	    :accessor loop-commands)))

(defun posix-while-until (test commands &optional is-until)
  (make-instance 'while-until-command :until is-until
		 :test test
		 :commands commands))

(defmethod run-command ((cmd while-until-command) &key &allow-other-keys)
  (let ((result 0))
    (with-redirected-io ((command-redirects cmd))
      (breakable-block
	(continuable-block
	  (print
	   (loop
	      do (mapc #'eval (loop-test cmd))
	      while (if (is-until cmd)
			(/= 0 (se-last-returnval *current-shell-environment*))
			(= 0 (se-last-returnval *current-shell-environment*)))
	      do (mapc #'eval (loop-commands cmd))
		(setf result (se-last-returnval *current-shell-environment*)))))))
    result))

(defmethod run-command ((cmd for-command) &key &allow-other-keys)
  (with-redirected-io ((command-redirects cmd))
    (breakable-block
      (loop for item in (for-command-words cmd)
	 do (continuable-block
	      (setf (iolib/os:environment-variable (for-command-name cmd)) item)
	      (mapc #'eval (for-command-commands cmd))))))
  (if (null (for-command-commands cmd))
      0
      (se-last-returnval *current-shell-environment*)))

(defclass case-command (command)
  ((testme :initform ""
	   :initarg :testme
	   :accessor case-command-testme
	   :type string)
   (patterns :initform nil
	     :initarg :patterns
	     :accessor case-command-patterns
	     :type list)))

(defmethod run-command ((cmd case-command) &key &allow-other-keys)
  (with-redirected-io ((command-redirects cmd))
    (let ((testme (car (first-expand-word (case-command-testme cmd) nil nil))))
      (loop for patterns in (case-command-patterns cmd)
	 for expanded-patterns = (first-expansions (car patterns) nil nil)
	 when
	   (loop for pattern in expanded-patterns
	      for regex = (glob-to-pcre (car (first-expand-word pattern nil nil)))
	      when (ppcre:scan regex testme) return t)
	 do (mapc #'eval (second patterns))
	 and return (se-last-returnval *current-shell-environment*)
	 finally (return 0)))))


(defmacro posix-case (testme &rest patterns)
    `(make-instance 'case-command
		    :testme ,`(quote ,testme)
		    :patterns ,`(quote ,patterns)))

(defun get-prompt ()
  (reap-children *current-shell-environment*)
  (car (first-expand-word (get-parameter "PS1") nil nil)))

(defun define-function (fname command redirects)
  (setf (gethash fname (se-functions *current-shell-environment*))
	(cons command redirects))
  (make-special-utility '(":") nil nil))
