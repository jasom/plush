;;;; plush.asd

(asdf:defsystem #:plush
  :serial t
  :description "sh implemented in common lisp"
  :author "Jason Miller <aidenn0@geocities.com>"
  :license "MIT"
  :depends-on (#:alexandria
	       #:esrap
	       #:cl-ppcre
               #:split-sequence
               #:iolib/syscalls
               #:iolib/pathnames
               #:iolib/os)
  :components ((:file "package")
               (:file "plush")
               (:file "plush-parser")))

