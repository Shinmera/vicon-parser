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
   #:frames
   
   #:frame
   #:id
   #:sub-id
   #:points
   #:sensor-point
   #:sensors
   
   #:sensor-point
   #:sensor
   #:data
   #:metrics
   #:field
   #:field-metric
   
   #:parse-field-for-metric
   #:parse-vicon-file))
