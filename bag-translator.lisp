(in-package #:vicon-bag-translator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *rst-path*
    (merge-pathnames
     #P"rst-proto/proto/stable/"
     (or (sb-posix:getenv "RST")
         #P"/home/linus/Projects/Bielefeld/rst/")))

  (defun load-proto-file (pathname)
    (let ((pbf:*proto-load-path* (list* *rst-path* pbf:*proto-load-path*)))
      (rsb.common:load-idl pathname :auto :purpose '(:packed-size :serializer :deserializer))))

  (defun load-vicon-proto-file ()
    (load-proto-file (asdf:system-relative-pathname :vicon-bag-translator "Vicon.proto")))

  (load-vicon-proto-file))

(defun make-precise-timestamp (universal)
  (multiple-value-bind (secs fractional-secs) (floor universal 1)
    (local-time:universal-to-timestamp secs :nsec (floor (* fractional-secs 1000000000)))))

(defgeneric to-vicon-object (object)
  (:method ((point marker-point))
    (alexandria:when-let* ((x (field :x point))
                           (y (field :y point))
                           (z (field :z point)))
      (make-instance
       'rst.devices.mocap:vicon/marker-point
       :name (string-downcase (marker point))
       :position (make-instance 'rst.math:vec3ddouble
                                :x (float x 1.0d0)
                                :y (float y 1.0d0)
                                :z (float z 1.0d0)))))
  (:method ((frame frame))
    (make-instance
     'RST.DEVICES.MOCAP:VICON
     :id (id frame)
     :sub-id (sub-id frame)
     :points (let ((array (make-array (hash-table-count (points frame))
                                      :fill-pointer 0)))
               (loop for point being the hash-values of (points frame)
                     for object = (to-vicon-object point)
                     when object do (vector-push object array))
               array))))

(defun translate-frame (frame channel id)
  (let* ((vicon (to-vicon-object frame))
         (timestamp (make-precise-timestamp
                     (timestamp frame))))
    (setf (rsbag:entry channel timestamp)
          (let ((event (rsb:make-event "/vicon/data" vicon)))
            (setf (rsb:event-sequence-number event) (rst.devices.mocap:vicon-id vicon)
                  (rsb:event-origin event) id
                  (rsb:timestamp event :create) timestamp
                  (rsb:timestamp event :send) timestamp
                  (rsb:timestamp event :receive) timestamp
                  (rsb:timestamp event :deliver) timestamp)
            event))))

(defun create-vicon-channel (bag id)
  (setf (rsbag:bag-channel bag "/vicon/data:.rst.devices.mocap.Vicon")
        `(:type (:rsb-event-0.8 :|.rst.devices.mocap.Vicon|)
          :source-name ,(princ-to-string id)
          :source-config ,(format nil "rsb:/#~A" id))))

(defun translate (vicon-csv-pathnames output-pathname)
  (rsbag:with-bag (bag output-pathname
                       :direction :io
                       :if-exists :supersede
                       :transform `(rsbag:&from-source
                                    :converter ,(cdr (assoc 'nibbles:octet-vector
                                                            (rsb:default-converters)))))
    (let* ((id (uuid:make-v1-uuid))
           (channel (create-vicon-channel bag id)))
      (dolist (vicon-csv-pathname vicon-csv-pathnames)
        (map-vicon-csv (lambda (key object)
                         (case key
                           (:frame (translate-frame object channel id))))
                       vicon-csv-pathname)))))
