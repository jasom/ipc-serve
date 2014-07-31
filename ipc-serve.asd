;;;; ipc-serve.asd

(asdf:defsystem #:ipc-serve
  :serial t
  :description "A simple socket job-server"
  :author "Jason Miller <jason@milr.com>"
  :license "MIT/X11"
  :depends-on (#:alexandria
               #:iolib/os
               #:iolib/pathnames
	       #:basic-binary-ipc
               #:lparallel)
  :components ((:file "package")
               (:file "ipc-serve")))

