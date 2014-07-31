(defpackage #:is-example
  (:use #:cl #:ipc-serve ))

(in-package #:is-example)


(defmethod start-job ((name (eql :add)) client job-id args)
  (list
   client
   job-id
   (apply #'+ args)))

(defun tnet-parse (buffer length eof-value)
  (multiple-value-bind
	(v l)
      (tnetstring:parse-tnetstring buffer 0 length)
    (if (eql v 'tnetstring::eof)
	(values eof-value 0)
	(values v l))))

(defun tnet-dump (obj)
  (tnetstring:dump-tnetstring obj))

(defun test-server ()
  ;(setf lparallel:*kernel*
	;(lparallel:make-kernel 10)
	;ipc-serve::*parse-function* #'tnet-parse)
  ;(with-socket (server (make-local-server (merge-pathnames #p"test-server2.socket" (user-homedir-pathname))))
    (server-loop (merge-pathnames #p"test-server2.socket" (user-homedir-pathname))
		 #'tnet-parse #'tnet-dump :type :local))
