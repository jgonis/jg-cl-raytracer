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
  (make-instance 'jg-point 
                 :x (coerce x 'float) 
                 :y (coerce y 'float) 
                 :z (coerce z 'float)))
(defun make-jg-vec (x y z)
  (make-instance 'jg-vec 
                 :x (coerce x 'float) 
                 :y (coerce y 'float) 
                 :z (coerce z 'float)))

(defun jg-point? (pt)
 (typep pt 'jg-point))
(defun jg-vec? (vec)
  (typep vec 'jg-vec))

(defgeneric equivalent (obj1 obj2))
(defmethod equivalent ((vec1 jg-vec) (vec2 jg-vec))
  (and (equivalent (x vec1) (x vec2))
       (equivalent (y vec1) (y vec2))
       (equivalent (z vec1) (z vec2))))
(defmethod equivalent ((pt1 jg-point) (pt2 jg-point))
  (and (equivalent (x pt1) (x pt2))
       (equivalent (y pt1) (y pt2))
       (equivalent (z pt1) (z pt2))))
(defmethod equivalent ((float1 float) (float2 float))
  (<= (abs (- float1 float2)) *epsilon*))

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
(defmethod subtract ((subend1 jg-vec) (subend2 jg-vec))
  (make-jg-vec (- (x subend1) (x subend2))
               (- (y subend1) (y subend2))
               (- (z subend1) (z subend2))))

(defgeneric negate (vec))
(defmethod negate ((vec jg-vec))
  (make-jg-vec (- (x vec))
               (- (y vec))
               (- (z vec))))

(defgeneric scale (vec scalar))
(defmethod scale ((vec jg-vec) (scalar number))
  (make-jg-vec (* scalar (x vec))
               (* scalar (y vec))
               (* scalar (z vec))))

(defgeneric div (vec dividend))
(defmethod div ((vec jg-vec) (dividend number))
  (make-jg-vec (/ (x vec) dividend)
               (/ (y vec) dividend)
               (/ (z vec) dividend)))

(defgeneric magnitude (vec))
(defmethod magnitude ((vec jg-vec))
  (sqrt (+ (expt (x vec) 2)
           (expt (y vec) 2)
           (expt (z vec) 2))))
