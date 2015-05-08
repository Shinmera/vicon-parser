(in-package #:vicon-parser)

(defmacro with-retry-restart ((name format-string &rest format-args) &body body)
  (let ((stream (gensym "STREAM"))
        (tag (gensym "TAG"))
        (block (gensym "BLOCK")))
    `(block ,block
       (tagbody
          ,tag
          (restart-case
              (return-from ,block
                (progn ,@body))
            (,name ()
              :report (lambda (,stream)
                        (format ,stream ,format-string ,@format-args))
              (go ,tag)))))))

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
   (markers :initarg :markers :accessor markers)
   (frames :initarg :frames :accessor frames))
  (:default-initargs
   :file (error "FILE required.")
   :description NIL
   :resolution (error "RESOLUTION required.")
   :markers (error "MARKERS required.")
   :frames (make-array 0 :adjustable T :fill-pointer 0)))

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
  ((vicon-file :initarg :vicon-file :accessor vicon-file)
   (id :initarg :id :accessor id)
   (sub-id :initarg :sub-id :accessor sub-id)
   (points :initarg :points :accessor points))
  (:default-initargs
   :vicon-file (error "VICON-FILE required.")
   :id (error "ID required.")
   :sub-id NIL
   :points (make-hash-table :test 'eql)))

(defmethod print-object ((frame frame) stream)
  (print-unreadable-object (frame stream :type T)
    (format stream "~d~@[/~d~]"
            (id frame) (sub-id frame))))

(defmethod timestamp ((frame frame))
  (float
   (+ (timestamp (vicon-file frame))
      (* (id frame) (/ 1 (resolution (vicon-file frame)))))
   1.0d0))

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

(defmethod markers ((frame frame))
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
  (let ((vars ())
        (markers ())
        (current (car markerlist)))
    (flet ((pushvar (field metric)
             (push (cons (translate-name field) (translate-name metric))
                   vars))
           (pushmarker (sitem)
             (push (list* (translate-name current) (nreverse vars))
                   markers)
             (setf current sitem
                   vars ())))
      (pushvar (car fieldlist) (car metriclist))
      (loop for sitem in (cdr markerlist)
            for vitem in (cdr fieldlist)
            for mitem in (cdr metriclist)
            when (string/= sitem "")
            do (pushmarker sitem)
            do (pushvar vitem mitem)
            finally (when vars (pushmarker NIL))))
    (nreverse markers)))

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

(defun parse-frame (data vicon-file)
  (let ((frame (make-instance 'frame
                              :vicon-file vicon-file
                              :id (parse-integer (first data))
                              :sub-id (parse-integer (second data))))
        (data (cddr data))) ; position data starts after id and sub-id
    (loop for (marker . fields) in (markers vicon-file)
          for point = (make-instance 'marker-point :marker marker)
          do (loop for (field . metric) in fields
                   for value = (pop data)
                   unless (alexandria:emptyp value)
                   do (setf (field field point) (parse-field-for-metric metric value)
                            (field-metric field point) metric))
         (setf (marker-point marker frame) point))
    frame))

(defun read-clean-line (stream)
  (alexandria:when-let ((line (read-line stream NIL)))
    (string-trim '(#\Return #\Newline #\Space) line)))

(defun split (char string)
  (let ((parts ())
        (output (make-string-output-stream)))
    (flet ((commit ()
             (push (get-output-stream-string output) parts)
             (setf output (make-string-output-stream))))
      (loop for current across string
            do (if (char= char current)
                   (commit)
                   (write-char current output))
            finally (commit)))
    (nreverse parts)))

(defun read-vicon-header (stream &key (file (ignore-errors (pathname stream))))
  (destructuring-bind (description resolution markers fields metrics)
      (loop repeat 5
            for line = (read-clean-line stream)
            if line collect line
            else do (error "Invalid Vicon file: incomplete header."))
    (make-instance
     'vicon-file
     :file file
     :description description
     :resolution (parse-integer resolution)
     :markers (parse-marker-list
               (cddr (split #\, markers))
               (cddr (split #\, fields))
               (cddr (split #\, metrics))))))

(defun map-read-vicon-frames (stream vicon-file function)
  (loop for line = (read-clean-line stream)
        while line
        do (unless (string= line "")
             (with-simple-restart (skip "Skip processing the line ~s" line)
               (let ((frame (with-retry-restart (retry "Retry parsing the line ~s" line)
                              (parse-frame (split #\, line) vicon-file))))
                 (with-retry-restart (retry "Retry calling the frame mapping function.")
                   (funcall function frame)))))))

(defun parse-vicon-file (pathname &key (external-format :default))
  (with-open-file (stream pathname :direction :input :external-format external-format)
    (let ((file (read-vicon-header stream)))
      (map-read-vicon-frames
       stream file
       (lambda (frame)
         (vector-push-extend frame (frames file))))
      file)))
