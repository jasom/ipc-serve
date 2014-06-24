;;;; redo2.asd

(asdf:defsystem #:ipc-serve
  :serial t
  :description "Describe redo2 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:iolib/os
               #:iolib/pathnames
	       #:basic-binary-ipc
               #:lparallel)
  :components ((:file "package")
               (:file "ipc-serve")))

