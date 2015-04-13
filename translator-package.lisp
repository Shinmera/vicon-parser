(in-package #:cl-user)
(defpackage #:vicon-bag-translator
  (:use #:cl #:vicon-parser)
  (:export
   #:*rst-path*
   #:load-proto-file
   #:load-vicon-proto-file
   #:to-vicon-object
   #:translate
   #:convert))
