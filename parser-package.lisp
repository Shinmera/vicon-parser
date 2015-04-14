(in-package #:cl-user)
(defpackage #:vicon-parser
  (:use #:cl)
  (:export
   #:translate-name

   #:vicon-file
   #:file
   #:timestamp
   #:description
   #:resolution

   #:frame
   #:id
   #:sub-id
   #:points
   #:marker-point
   #:markers

   #:marker-point
   #:marker
   #:data
   #:metrics
   #:field
   #:field-metric

   #:map-vicon-csv))
