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
   #:markers
   
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
   
   #:parse-field-for-metric
   #:read-vicon-header
   #:map-read-vicon-frames
   #:parse-vicon-file))
