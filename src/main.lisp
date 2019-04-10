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

(defparameter *epsilon* 0.0000001)

(defun make-jg-point (x y z)
  (make-instance 'jg-point :x x :y y :z z))
(defun make-jg-vec (x y z)
  (make-instance 'jg-vec :x x :y y :z z))
(defun jg-point? (pt)
 (typep pt 'jg-point))
(defun jg-vec? (vec)
  (typep vec 'jg-vec)
)
(defgeneric equivalent (obj1 obj2))
(defmethod equivalent ((vec1 jg-vec) (vec2 jg-vec))
  (and (<= (abs (- (x vec1) (x vec2))) *epsilon*)
       (<= (abs (- (y vec1) (y vec2))) *epsilon*)
       (<= (abs (- (z vec1) (z vec2))) *epsilon*)))

(defmethod equivalent ((pt1 jg-point) (pt2 jg-point))
  (and (<= (abs (- (x pt1) (x pt2))) *epsilon*)
       (<= (abs (- (y pt1) (y pt2))) *epsilon*)
       (<= (abs (- (z pt1) (z pt2))) *epsilon*)))

(defgeneric add (addend1 addend2))
(defmethod add ((addend1 jg-point) (addend2 jg-vec))
  (make-jg-point (+ (x addend1) (x addend2))
                 (+ (y addend1) (y addend2))
                 (+ (z addend1) (z addend2))))
(defmethod add ((addend1 jg-vec) (addend2 jg-vec))
  (make-jg-vec (+ (x addend1) (x addend2))
               (+ (y addend1) (y addend2))
               (+ (z addend1) (z addend2))))
(defgeneric subtract (subend1 subend2))
(defmethod subtract ((subend1 jg-point) (subend2 jg-point))
  (make-jg-vec (- (x subend1) (x subend2))
               (- (y subend1) (y subend2))
               (- (z subend1) (z subend2))))
(defmethod subtract ((subend1 jg-point) (subend2 jg-vec))
  (make-jg-point (- (x subend1) (x subend2))
                 (- (y subend1) (y subend2))
                 (- (z subend1) (z subend2))))
