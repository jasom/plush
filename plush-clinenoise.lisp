(in-package :plush)

(defun ansi-length (string)
  (loop
    with mode = :normal
    for char across string
    if (eql mode :normal)
    if (char= char #\Esc) do (setf mode :escape)
    else sum 1
    else if (eql mode :escape)
    if (char= char #\[) do (setf mode :code)
    else sum 1
    else
    if (eql mode :code)
    if (member char '(#\H #\f #\A #\B #\C #\D #\s #\u #\J #\K #\m
                      #\h #\l #\p))
    do (setf mode :normal)
    end
    else sum 1))

(defun linenoise-and-parse-posix-stuff ()
  (prog ()
     restart
     (return
       (loop
	  with rv
	  for prompt = (get-prompt)
	  for line = (clinenoise::linenoise prompt (ansi-length prompt))
	  then (clinenoise::linenoise "> ")
	  for sofar = (and (not (eql line :eof)) (format nil "~A~%" line))
	  then (concatenate 'string sofar (format nil "~A~%" line))
	  when (string= sofar (string #\Newline)) do (go restart)
	  when (eql line :eof) return :eof
	  when (and
		(not (char= (char sofar (- (length sofar) 2)) #\\))
		(setf rv
		      (handler-case (plush-parser::parse-posix-stuff sofar)
			(plush-parser::eof-when-tokenizing nil)
			(plush-parser::posix-parse-failed nil))))
	  do (clinenoise::history-add (coerce (butlast (coerce sofar 'list)) 'string)) (return rv)))))
     

(defun posix-linenoise ()
  (clinenoise::set-completion-callback 'linenoise-complete)
  (clinenoise::history-load (concatenate 'string (iolib/os:environment-variable "HOME") "/.plush_history"))
  (setf clinenoise::*multiline* 1)
  ;(clinenoise::set-multiline 1)
  (loop
     for expr = (linenoise-and-parse-posix-stuff)
     until (eql expr :eof)
     do (mapc #'eval expr)))

(defun linenoise-complete (buf)
  (when (not (position #\Space buf))
    (let ((paths (split-sequence:split-sequence #\: (iolib/os:environment-variable "PATH"))))
      (loop for path in paths
           nconc
           (delete nil
                   (ignore-errors
                     (iolib/os:mapdir
                       (lambda (x)
                         (let ((x (princ-to-string x)))
                           (when (starts-with-subseq buf x)
                             x)))
                       path)))))))

