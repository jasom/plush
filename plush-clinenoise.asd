;;;; plush-linedit.asd

(asdf:defsystem #:plush-clinenoise
  :serial t
  :description "sh implemented in common lisp"
  :author "Jason Miller <aidenn0@geocities.com>"
  :license "MIT"
  :depends-on (#:plush
	       #:clinenoise)
  :components ( (:file "plush-clinenoise")))

