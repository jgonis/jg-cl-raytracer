(in-package :jg-cl-geom)
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

(defclass jg-matrix ()
  ((data :initarg :data
         :reader data)
   (rows :initarg :rows
         :reader rows)
   (columns :initarg :columns
            :reader columns)))

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

(defun make-jg-matrix (rows columns)
  (let* ((cols (coerce columns 'fixnum))
         (rws (coerce rows 'fixnum))
         (arr (make-array (list rws cols)
                          :element-type 'float
                          :initial-element 0.0)))
    (make-instance 'jg-matrix
                   :rows (coerce rows 'fixnum)
                   :columns cols
                   :data arr)))
  
(defun jg-point? (pt)
  (typep pt 'jg-point))
(defun jg-vec? (vec)
  (typep vec 'jg-vec))
(defun jg-matrix? (mat)
  (typep mat 'jg-matrix))

(defmethod print-object ((pt jg-point) strm)
  (format strm 
          "Point x:~A y:~A z:~A~%" 
          (x pt)
          (y pt)
          (z pt)))
(defmethod print-object ((vec jg-vec) strm)
  (format strm 
          "Vector x:~A y:~A z:~A~%" 
          (x vec)
          (y vec)
          (z vec)))
(defmethod print-object ((mat jg-matrix) strm)
  (format strm "Matrix:~%")
  (let ((data (data mat))
        (rows (rows mat))
        (cols (columns mat)))
    (dotimes (i rows) 
      (dotimes (j cols)
        (format strm "~A " (aref data i j)))
      (format strm "~%"))))

(defmethod equivalent ((vec1 jg-vec) (vec2 jg-vec))
  (and (equivalent (x vec1) (x vec2))
       (equivalent (y vec1) (y vec2))
       (equivalent (z vec1) (z vec2))))
(defmethod equivalent ((pt1 jg-point) (pt2 jg-point))
  (and (equivalent (x pt1) (x pt2))
       (equivalent (y pt1) (y pt2))
       (equivalent (z pt1) (z pt2))))
(defmethod equivalent ((mat1 jg-matrix) (mat2 jg-matrix))
  (null (mismatch (data mat1) (data mat2))))

(defun element-at (matrix row column)
  (aref (data matrix) row column))
(defun (setf element-at) (new-value matrix row column)
  (setf (aref (data matrix) row column) new-value))

(defmethod add ((addend1 jg-point) (addend2 jg-vec))
  (make-jg-point (+ (x addend1) (x addend2))
                 (+ (y addend1) (y addend2))
                 (+ (z addend1) (z addend2))))
(defmethod add ((addend1 jg-vec) (addend2 jg-vec))
  (make-jg-vec (+ (x addend1) (x addend2))
               (+ (y addend1) (y addend2))
               (+ (z addend1) (z addend2))))

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

(defgeneric normalize (vec))
(defmethod normalize ((vec jg-vec))
  (let ((mag (magnitude vec)))
    (make-jg-vec (/ (x vec) mag)
                 (/ (y vec) mag)
                 (/ (z vec) mag))))

(defgeneric dot-product (vec1 vec2))
(defmethod dot-product ((vec1 jg-vec) (vec2 jg-vec))
  (+ (* (x vec1) (x vec2))
     (* (y vec1) (y vec2))
     (* (z vec1) (z vec2))))

(defgeneric cross-prod (vec1 vec2))
(defmethod cross-prod ((vec1 jg-vec) (vec2 jg-vec))
  (make-jg-vec (- (* (y vec1) (z vec2)) 
                  (* (z vec1) (y vec2)))
               (- (* (z vec1) (x vec2))
                  (* (x vec1) (z vec2)))
               (- (* (x vec1) (y vec2))
                  (* (y vec1) (x vec2)))))
