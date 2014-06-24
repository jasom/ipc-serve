;;;; package.lisp

(defpackage #:ipc-serve
  (:use #:cl #:lparallel #:basic-binary-ipc #:alexandria)
  (:export #:server-loop #:start-job))

