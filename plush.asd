;;;; plush.asd

(asdf:defsystem #:plush
  :serial t
  :description "Describe plush here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
	       #:smug
	       #:cl-ppcre
               #:split-sequence
               #:iolib/syscalls
               #:iolib/pathnames
               #:iolib/os)
  :components ((:file "package")
               (:file "plush")
               (:file "plush-parser")))

