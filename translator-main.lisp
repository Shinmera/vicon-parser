(cl:in-package #:vicon-bag-translator)

(defun main ()
  (destructuring-bind (input &optional (output (make-pathname
						:type     "tide"
						:defaults input)))
      (uiop:command-line-arguments)
    (convert input output)))
