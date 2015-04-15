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

(defmacro with-default-bag ((bag) pathname &body body)
  `(rsbag:with-bag (,bag ,pathname
                         :direction :io
                         :if-exists :supersede
                         :transform `(rsbag:&from-source
                                      :converter ,(cdr (assoc 'nibbles:octet-vector
                                                              (rsb:default-converters)))))
     ,@body))

(defun call-with-vicon-channel (bag-or-pathname function)
  (etypecase bag-or-pathname
    ((or string pathname)
     (with-default-bag (bag) bag-or-pathname
       (call-with-vicon-channel bag function)))
    (rsbag:bag
     (let* ((id (uuid:make-v1-uuid))
            (channel (create-vicon-channel bag-or-pathname id)))
       (funcall function id channel)))))

(defmacro with-vicon-channel ((channel id) bag-or-pathname &body body)
  `(call-with-vicon-channel ,bag-or-pathname (lambda (,id ,channel)
                                               ,@body)))

(defun translate (vicon-file-descriptor channel id)
  (flet ((process-stream (stream)
           (let ((file (read-vicon-header stream)))
             (map-read-vicon-frames
              stream file
              (lambda (frame)
                (translate-frame frame channel id))))))
    (etypecase vicon-file-descriptor
      ((or pathname string)
       (with-open-file (stream vicon-file-descriptor :direction :input)
         (process-stream stream)))
      (stream
       (process-stream vicon-file-descriptor))
      (vicon-file
       (loop for frame across (frames vicon-file-descriptor)
             do (translate-frame frame channel id))))))

(defun convert (source target)
  (with-vicon-channel (channel id) target
    (dolist (src (alexandria:ensure-list source))
      (translate src channel id))))

(defun main (&rest noop)
  (declare (ignore noop))
  (let ((args (uiop:command-line-arguments)))
    (case (length args)
      ((0 1)
       (print-help))
      (T
       (let ((args (mapcar #'uiop:parse-native-namestring args)))
         (convert (rest args) (first args)))))))

(defun print-help ()
  (let ((system (asdf:find-system :vicon-bag-translator)))
    (format T "~a v~a

~a

Usage:
vicon-bag-translator output input [input ...]

  output        Path to the resulting TIDE file
  input         Path to a Vicon CSV input file

If multiple input files are specified, they are
processed into the bag in sequence on the same
channel.

Project URL: ~a
Maintend by: ~a
Compiled against
~{~a~^, ~}"
            (asdf:component-name system)
            (asdf:component-version system)
            (asdf:system-description system)
            (asdf:system-homepage system)
            (asdf:system-maintainer system)
            (mapcar (lambda (dep)
                      (let ((system (asdf:find-system dep)))
                        (format NIL "~a v~a"
                                (asdf:component-name system)
                                (asdf:component-version system))))
                    (asdf:system-depends-on system)))))
