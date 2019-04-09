(in-package :jg-cl-raytracer)

(defclass jg-point ()
  ((x :initarg :x
      :reader x)
   (y :initarg :y
      :reader y)
   (z :initarg :z
      :reader z)
   (w :initform 1
      :reader w)))

(defclass jg-vec ()
  ((x :initarg :x 
      :reader x)
   (y :initarg :y
      :reader y)
   (z :initarg :z 
      :reader z)
   (w :initform 0
      :reader w)))

(defun make-jg-point (x y z)
  (make-instance 'jg-point :x x :y y :z z))
(defun make-jg-vec (x y z)
  (make-instance 'jg-vec :x x :y y :z z))
(defun jg-point? (pt)
 (typep pt 'jg-point))
(defun jg-vec? (vec)
  (typep vec 'jg-vec))
