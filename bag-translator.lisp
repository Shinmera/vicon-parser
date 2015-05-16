(in-package #:vicon-bag-translator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-proto-file (asdf:system-relative-pathname :vicon-bag-translator "Vicon.proto")))

(defmethod to-vicon-object ((point marker-point))
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
(defmethod to-vicon-object ((frame frame))
  (make-instance
   'RST.DEVICES.MOCAP:VICON
   :id (id frame)
   :sub-id (sub-id frame)
   :points (let ((array (make-array (hash-table-count (points frame))
                                    :fill-pointer 0)))
             (loop for point being the hash-values of (points frame)
                   for object = (to-vicon-object point)
                   when object do (vector-push object array))
             array)))

(defmacro with-vicon-channel ((channel bag) &body body)
  `(with-bag-channel (,channel ,bag "/vicon/data" ".rst.devices.mocap.Vicon")
     ,@body))

(defun convert (source target)
  (with-default-bag (bag target)
    (with-vicon-channel (channel bag)
      (with-file-descriptor (stream file source)
        (let ((file (apply #'read-vicon-header stream
                           (when file (list :file file)))))
          (map-read-vicon-frames
           stream file
           (lambda (frame)
             (make-entry channel frame (make-precise-timestamp (timestamp frame))
                         :sequence-number (id frame)))))))))

(defun print-help ()
  (let ((system (asdf:find-system :vicon-bag-translator)))
    (format T "~a v~a

~a

Usage:
vicon-bag-translator output input [input ...]

  output        Path to the resulting TIDE file
  input         Path to a Vicon CSV input file
                (can be a Zip-file containing the CSV file)

If multiple input files are specified, they are
processed into the bag in sequence on the same
channel.

Project URL:   ~a
Maintained by: ~a
Compiled against
  ~@<~{~{~36a ~a~}~^~@:_~}~@:>
"
            (asdf:component-name system)
            (asdf:component-version system)
            (asdf:system-description system)
            (asdf:system-homepage system)
            (asdf:system-maintainer system)
            (mapcar (lambda (dep)
                      (let ((system (asdf:find-system dep)))
                        (list (asdf:component-name system)
                              (asdf:component-version system))))
                    (asdf:system-depends-on system)))))

(defun main (&rest noop)
  (declare (ignore noop))
  (let ((args (uiop:command-line-arguments)))
    (case (length args)
      ((0 1)
       (print-help))
      (T
       (let ((args (mapcar #'uiop:parse-native-namestring args)))
         (convert (rest args) (first args)))))))
