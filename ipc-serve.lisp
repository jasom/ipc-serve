;;; redo2.lisp

(in-package #:ipc-serve)

;;; "redo2" goes here. Hacks and glory await!

(defvar *parse-function*)
(defvar *dump-function*)
(defvar *job-channel*)
(defvar *buffer-size* 2048)
(defparameter *clients* nil)
(defparameter *client-ids* 0)

(defgeneric start-job (name client job-id args))

(defstruct client
  (events (list 'remote-disconnected-p 'data-available-p))
  (socket nil)
  (id (incf *client-ids*))
  (direction :in)
  (inbuf (make-array *buffer-size* :element-type '(unsigned-byte 8)))
  (in-full 0)
  (outbuf nil)
  (out-full 0))

(defun handle-new-connection (server)
  (let ((socket (accept-connection server)))
    (push (make-client :socket socket) *clients*)))

(defun try-to-run (client)
  (handler-case
      (loop
	 (multiple-value-bind (value size)
	     (funcall *parse-function*
		      (client-inbuf client)
		      (client-in-full client)
		      'eof)
	   (when (eql value 'eof)
	     (return))
	   (unless
	       (and
		(listp value) ;it's a list
		(cdr value)  ;of at least 2 elements
		(typep (first value) 'vector)   ;name (byte-array)
		(typep (second value) 'integer)) ;id (integer)
	     (error 'parse-error))
	   (let ((kwd (make-keyword (map 'string #'code-char (first value)))))
	     (submit-task *job-channel* 'start-job kwd client (second value) (cddr value)))
	   (if (= size (client-in-full client))
	       (setf (client-in-full client) 0)
	       (progn
		 (loop
		    for high from size below (client-in-full client)
		    for low from 0
		    do (setf (aref (client-inbuf client) low)
			     (aref (client-inbuf client) high)))
		 (decf (client-in-full client) size)))))
    (parse-error ()
      (close-socket (client-socket client))
      (setf *clients* (delete client *clients*)))))

(defun read-some-data (client)
  (ignore-errors
    (let ((bytes
	   (read-from-stream (client-socket client) (client-inbuf client) :end (length (client-inbuf client)))))
      (when bytes
	(when (= (incf (client-in-full client) bytes) (length (client-inbuf client)))
	  (setf (client-inbuf client)
		(replace
		 (make-array (* (length (client-inbuf client)) 2) :element-type '(unsigned-byte 8))
		 (client-inbuf client))))))))

(defun client-work (client events)
  (when events
    (let ((events (ensure-list events)))
      (if (member 'remote-disconnected-p events)
	  (setf *clients* (delete client *clients*))
	  (progn
	    (when (member 'data-available-p events)
	      (read-some-data client)
	      (try-to-run client))
	    (when (member 'ready-to-write-p events)
	      (send-some-data client)))))))

(defun send-some-data (client)
  (cond
    ((null (client-outbuf client))
     (setf (client-events client)
	   (remove 'ready-to-write-p (client-events client))))
    (t
     (let ((bytes
	    (write-to-stream (client-socket client) (car (client-outbuf client)) :start (client-out-full client))))
       (when bytes
	 (when (= (incf (client-out-full client) bytes) (length (car (client-outbuf client))))
	   (setf (client-out-full client) 0)
	   (pop (client-outbuf client))))))))

(defun send-result (client job-id value)
    (if (client-outbuf client)
	(setf (cdr (client-outbuf client))
	      (cons (funcall *dump-function* (list job-id value))
		    (cdr (client-outbuf client))))
	(progn
	  (push (funcall *dump-function* (list job-id value)) (client-outbuf client))
	  (pushnew 'ready-to-write-p (client-events client)))
	  ))

(defun server-loop (connect-to parse-fn dump-fn
		    &key (type :local) (threads 10))
  (setf
	lparallel:*kernel*
	*job-channel* (make-channel)
	(lparallel:make-kernel threads)
	*parse-function* parse-fn
	*dump-function* dump-fn)
  (with-socket
      (server
       (ecase type
	 (:local (make-local-server connect-to))
	 (:ipv4 (make-ipv4-tcp-server
		 (resolve-ipv4-address (car connect-to))
		 (second connect-to)))))
    (loop
       for (server-e . clients-e) = (poll-sockets (cons server (mapcar #'client-socket  *clients*))
						  (cons 'connection-available-p (mapcar #'client-events *clients*))
						  1)
       when server-e do (handle-new-connection server)
       do (mapc #'client-work *clients* clients-e)
	 (loop for (result present) = (multiple-value-list (try-receive-result *job-channel*))
	    while present
	    do
	      (send-result
	       (first result)  ;client
	       (second result) ;job-id
	       (third result)) ;value
	      ))))
