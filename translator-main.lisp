(in-package #:vicon-bag-translator)

(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (inputs (butlast args))
         (output (alexandria:lastcar args)))
    (convert inputs output)))
