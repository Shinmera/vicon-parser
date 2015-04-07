(in-package #:vicon-parser)

(defun translate-name (name)
  (flet ((r (search replace data)
           (cl-ppcre:regex-replace-all search data replace)))
    (intern (string-upcase
             (r ":" "-" (r "_" "-" name)))
            "KEYWORD")))

(defun parse-timestamp (string)
  (let ((year   (parse-integer string :start 0 :end 4))
        (month  (parse-integer string :start 4 :end 6))
        (day    (parse-integer string :start 6 :end 8))
        (hour   (parse-integer string :Start 9 :end 11))
        (minute (parse-integer string :start 11 :end 13))
        (second (parse-integer string :start 13 :end 15)))
    (encode-universal-time second minute hour day month year)))

(defclass vicon-file ()
  ((file :initarg :file :accessor file)
   (timestamp :initarg :timestamp :accessor timestamp)
   (description :initarg :description :accessor description)
   (resolution :initarg :resolution :accessor resolution)
   (frames :initarg :frames :accessor frames))
  (:default-initargs
   :file (error "FILE required.")
   :description NIL
   :resolution (error "RESOLUTION required.")
   :frames ()))

(defmethod initialize-instance :after ((file vicon-file) &key)
  (unless (slot-boundp file 'timestamp)
    (setf (timestamp file)
          (parse-timestamp (subseq (pathname-name (file file))
                                   (length "vicon_")
                                   (length "vicon_YYYYMMDD_hhmmss"))))))

(defmethod print-object ((file vicon-file) stream)
  (print-unreadable-object (file stream :type T)
    (format stream "~a (~d frames @~dHz) ~a"
            (description file)
            (length (frames file))
            (resolution file)
            (pathname-name (file file)))))

(defclass frame ()
  ((id :initarg :id :accessor id)
   (sub-id :initarg :sub-id :accessor sub-id)
   (points :initarg :points :accessor points))
  (:default-initargs
   :id (error "ID required.")
   :sub-id NIL
   :points (make-hash-table :test 'eql)))

(defmethod print-object ((frame frame) stream)
  (print-unreadable-object (frame stream :type T)
    (format stream "~d~@[/~d~]"
            (id frame) (sub-id frame))))

(defgeneric marker-point (name frame)
  (:method ((name string) (frame frame))
    (marker-point (translate-name name) frame))
  (:method ((name symbol) (frame frame))
    (gethash name (points frame))))

(defgeneric (setf marker-point) (point name frame)
  (:method (point (name string) (frame frame))
    (setf (marker-point (translate-name name) frame) point))
  (:method (point (name symbol) (frame frame))
    (setf (gethash name (points frame)) point)))

(defun markers (frame)
  (loop for marker being the hash-keys of (points frame)
        collect marker))

(defclass marker-point ()
  ((marker :initarg :marker :accessor marker)
   (data :initarg :data :accessor data)
   (metrics :initarg :metrics :accessor metrics))
  (:default-initargs
   :marker (error "MARKER required.")
   :data (make-hash-table :test 'eql)
   :metrics (make-hash-table :test 'eql)))

(defmethod print-object ((point marker-point) stream)
  (print-unreadable-object (point stream :type T)
    (format stream "~a ~{~a~^ ~}"
            (marker point)
            (loop for k being the hash-keys of (data point)
                  for v being the hash-values of (data point)
                  collect (cons k v)))))

(defgeneric field (name point)
  (:method ((name string) (point marker-point))
    (field (translate-name name) point))
  (:method ((name symbol) (point marker-point))
    (gethash name (data point))))

(defgeneric (setf field) (value name point)
  (:method (value (name string) (point marker-point))
    (setf (field (translate-name name) point) value))
  (:method (value (name symbol) (point marker-point))
    (setf (gethash name (data point)) value)))

(defgeneric field-metric (name point)
  (:method ((name string) (point marker-point))
    (field-metric (translate-name name) point))
  (:method ((name symbol) (point marker-point))
    (gethash name (metrics point))))

(defgeneric (setf field-metric) (value name point)
  (:method (value (name string) (point marker-point))
    (setf (field-metric (translate-name name) point) value))
  (:method (value (name symbol) (point marker-point))
    (setf (gethash name (metrics point)) value)))

(defun parse-marker-list (markerlist fieldlist metriclist)
  (let ((vars ()))
    (flet ((pushvar (field metric)
             (push (cons (translate-name field)
                         (translate-name metric))
                   vars)))
      (pushvar (car fieldlist) (car metriclist))
      (loop with marker = (car markerlist)
            for sitem in (cdr markerlist)
            for vitem in (cdr fieldlist)
            for mitem in (cdr metriclist)
            when (string/= sitem "")
            collect (prog1 (cons (translate-name marker)
                                 (nreverse vars))
                      (setf marker sitem
                            vars ()))
            do (pushvar vitem mitem)))))

(defgeneric parse-field-for-metric (metric field)
  (:method ((metric string) field)
    (parse-field-for-metric (translate-name metric) field))
  (:method ((metric (eql :float)) field)
    (parse-float:parse-float field :junk-allowed T))
  (:method ((metric (eql :mm)) field)
    (let ((mm (parse-field-for-metric :float field)))
      (/ mm 1000)))
  (:method ((metric (eql :cm)) field)
    (let ((cm (parse-field-for-metric :float field)))
      (/ cm 100)))
  (:method ((metric (eql :m)) field)
    (parse-field-for-metric :float field)))

(defun parse-frame (data markers)
  (let ((frame (make-instance 'frame :id (parse-integer (first data))
                                     :sub-id (parse-integer (second data))))
        (data (cddr data)))
    (loop for (marker . fields) in markers
          for point = (make-instance 'marker-point :marker marker)
          do (loop for (field . metric) in fields
                   do (setf (field field point) (parse-field-for-metric metric (pop data))
                            (field-metric field point) metric))
             (setf (marker-point marker frame) point))
    frame))

(defun parse-frames (data markers)
  (let ((frames (make-array (length data) :adjustable T :fill-pointer 0)))
    (dolist (record data)
      (vector-push-extend (parse-frame (cl-ppcre:split "," record) markers) frames))
    frames))

(defun parse-vicon-file (pathname &key external-format)
  (cl-ppcre:register-groups-bind (description resolution markers fields metrics data)
      ("(.*?)[\\r\\n]+([0-9]*)[\\r\\n]+(.*?)[\\r\\n]+(.*?)[\\r\\n]+(.*?)[\\r\\n]+([\\s\\S]*)"
       (alexandria:read-file-into-string pathname :external-format external-format))
    (declare (ignore data))
    (let ((file (make-instance 'vicon-file :file pathname
                                           :description description
                                           :resolution (parse-integer resolution)))
          (markers (parse-marker-list
                    (cddr (cl-ppcre:split "," markers))
                    (cddr (cl-ppcre:split "," fields))
                    (cddr (cl-ppcre:split "," metrics)))))
      (setf (frames file)
            (parse-frames
             (cl-ppcre:split "[\\r\\n]+" data)
             markers))
      file)))
