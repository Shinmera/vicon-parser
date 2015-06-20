(in-package #:cl-user)
(defpackage #:vicon-bag-translator
  (:use #:cl #:vicon-parser #:rsbag-helper)
  (:shadowing-import-from #:vicon-parser #:id)
  (:export
   #:with-vicon-channel
   #:convert
   #:main))
