(in-package :jg-cl-utils)
(defparameter *epsilon* 0.000001)
(defgeneric equivalent (obj1 obj2))
(defmethod equivalent ((float1 float) (float2 float))
  (<= (abs (- float1 float2)) *epsilon*))
(defgeneric add (addend1 addend2))
(defgeneric subtract (subend1 subend2))
(defgeneric scale (vec scalar))

(defun make-growable-vector (initial-size) 
    (make-array initial-size :fill-pointer 0 :adjustable t))

(defgeneric read-lines-to-sequence (input))
(defmethod read-lines-to-sequence ((input stream))
  (let ((seq (make-growable-vector 0)))
    (do ((line (read-line input nil) (read-line input nil)))
        ((null line) seq)
      (vector-push-extend line seq))
    seq))
(defmethod read-lines-to-sequence ((input string))
  (let ((strm (make-string-input-stream input)))
    (read-lines-to-sequence strm)))

(defun wrap-lines-longer-than (str wrap-length)
  (let ((out-stream (make-string-output-stream)))
    (if (< (length str) wrap-length)
        (format out-stream "~A" str)
        (let ((pos (position #\Space 
                             str 
                             :from-end t 
                             :end wrap-length)))
          (format out-stream 
                  "~A~%~A" 
                  (subseq str 0 pos)
                  (wrap-lines-longer-than (subseq str (+ 1 pos))
                                          wrap-length))))
    (get-output-stream-string out-stream)))
