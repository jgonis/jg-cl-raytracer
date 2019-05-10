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

(defun make-jg-matrix (rows columns &key data)
  (let* ((cols (coerce columns 'fixnum))
         (rws (coerce rows 'fixnum))
         (arr (make-array (list rws cols)
                          :element-type 'float
                          :initial-element 0.0)))
    (cond ((not (null data))
           (if (not (= (length data) (* rows columns)))
               (warn 
                "Not enough elements in data to populate matrix")
               (dotimes (i rws)
                 (dotimes (j cols)
                   (setf (aref arr i j) 
                         (coerce (elt data (+ j (* i cols)))
                                 'float)))))))
    (make-instance 'jg-matrix
                   :rows (coerce rows 'fixnum)
                   :columns (coerce cols 'fixnum)
                   :data arr)))
(defun make-identity-matrix (rows columns)
  (cond ((not (= (coerce rows 'fixnum) 
                 (coerce columns 'fixnum)))
         (error "Identity matrix must have == row and column size")))
  (let ((mat (make-jg-matrix rows columns)))
    (dotimes (i (rows mat) mat)
      (setf (element-at mat i i) 1.0))))
(defun make-translation-matrix (x-trans y-trans z-trans)
  (let ((trans-mat (make-identity-matrix 4 4)))
    (setf (element-at trans-mat 0 3) (coerce x-trans 'float))
    (setf (element-at trans-mat 1 3) (coerce y-trans 'float))
    (setf (element-at trans-mat 2 3) (coerce z-trans 'float))
    trans-mat))
(defun make-scaling-matrix (x-scale y-scale z-scale)
  (let ((x-scale-f (coerce x-scale 'float))
        (y-scale-f (coerce y-scale 'float))
        (z-scale-f (coerce z-scale 'float)))
    (make-jg-matrix 4
                    4
                    :data (list x-scale-f 0 0 0 
                                0 y-scale-f 0 0 
                                0 0 z-scale-f 0 
                                0 0 0 1))))
(defun make-x-rotation-matrix (x-rad)
  (let* ((x-rad-f (coerce x-rad 'float))
         (cos-x (cos x-rad-f))
         (sin-x (sin x-rad-f)))
    (make-jg-matrix 4
                    4
                    :data (list 1 0 0 0 
                                0 cos-x (- sin-x) 0
                                0 sin-x cos-x 0
                                0 0 0 1))))
(defun make-y-rotation-matrix (y-rad)
  (let* ((y-rad-f (coerce y-rad 'float))
         (cos-y (cos y-rad-f))
         (sin-y (sin y-rad-f)))
    (make-jg-matrix 4
                    4
                    :data (list cos-y 0 sin-y 0
                                 0 1 0 0
                                 (- sin-y) 0 cos-y 0
                                 0 0 0 1))))
(defun make-z-rotation-matrix (z-rad)
  (let* ((z-rad-f (coerce z-rad 'float))
         (cos-z (cos z-rad-f))
         (sin-z (sin z-rad-f)))
    (make-jg-matrix 4
                    4
                    :data (list cos-z (- sin-z) 0 0
                                sin-z cos-z 0 0
                                0 0 1 0
                                0 0 0 1))))
(defun make-shearing-matrix (x-y x-z y-x y-z z-x z-y)
  (let* ((x-y-f (coerce x-y 'float))
         (x-z-f (coerce x-z 'float))
         (y-x-f (coerce y-x 'float))
         (y-z-f (coerce y-z 'float))
         (z-x-f (coerce z-x 'float))
         (z-y-f (coerce z-y 'float)))
    (make-jg-matrix 4
                    4
                    :data (list 1 x-y-f x-z-f 0
                                y-x-f 1 y-z-f 0
                                z-x-f z-y-f 1 0
                                0 0 0 1))))
  
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
  (and (= (rows mat1) (rows mat2))
       (= (columns mat1) (columns mat2))
       (dotimes (i (rows mat1) t)
         (if (null (dotimes (j (columns mat1) t)
                     (if (not (equivalent (element-at mat1 i j)
                                          (element-at mat2 i j)))
                         (return nil))))
             (return nil)))))

(defgeneric element-at (matrix row column))
(defmethod element-at ((matrix jg-matrix) 
                       (row fixnum) 
                       (column fixnum))
  (aref (data matrix) row column))
(defmethod (setf element-at) ((new-value float) 
                              (matrix jg-matrix) 
                              (row fixnum) 
                              (column fixnum))
  (setf (aref (data matrix) row column) (coerce new-value 'float)))

(defgeneric transpose (mat))
(defmethod transpose ((mat jg-matrix))
  (let ((result (make-jg-matrix (columns mat) (rows mat))))
    (dotimes (i (rows mat) result)
      (dotimes (j (columns mat) result)
        (setf (element-at result j i) 
              (element-at mat i j))))))

(defmethod multiply ((mat1 jg-matrix) (mat2 jg-matrix))
  (let* ((rows (rows mat1))
         (cols (columns mat2)) 
         (result-mat (make-jg-matrix rows cols)))
    (cond ((not (= (columns mat1) (rows mat2)))
           (error "Matrix dimension mismatch. A has ~A columns and B has ~A rows" 
                  (columns mat1) 
                  (rows mat2))))
    (dotimes (row rows result-mat)
      (dotimes (col cols result-mat)
        (let ((result 0))
          (dotimes (count rows)
            (setf result (+ (* (element-at mat1 row count)
                               (element-at mat2 count col))
                            result)))
            (setf (element-at result-mat row col)
                  result))))))
(defmethod multiply ((mat jg-matrix) (pt jg-point))
  (let* ((xv (x pt))
        (yv (y pt))
        (zv (z pt))
        (wv 1)
        (res-x (+ (* xv (element-at mat 0 0))
                  (* yv (element-at mat 0 1))
                  (* zv (element-at mat 0 2))
                  (* wv (element-at mat 0 3))))
        (res-y (+ (* xv (element-at mat 1 0))
                  (* yv (element-at mat 1 1))
                  (* zv (element-at mat 1 2))
                  (* wv (element-at mat 1 3))))
        (res-z (+ (* xv (element-at mat 2 0))
                  (* yv (element-at mat 2 1))
                  (* zv (element-at mat 2 2))
                  (* wv (element-at mat 2 3)))))
    (make-jg-point res-x res-y res-z)))
(defmethod multiply ((mat jg-matrix) (vec jg-vec))
  (let* ((xv (x vec))
         (yv (y vec))
         (zv (z vec))
         (res-x (+ (* xv (element-at mat 0 0))
                   (* yv (element-at mat 0 1))
                   (* zv (element-at mat 0 2))))
         (res-y (+ (* xv (element-at mat 1 0))
                   (* yv (element-at mat 1 1))
                   (* zv (element-at mat 1 2))))
         (res-z (+ (* xv (element-at mat 2 0))
                   (* yv (element-at mat 2 1))
                   (* zv (element-at mat 2 2)))))
    (make-jg-vec res-x res-y res-z)))

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

(defgeneric determinant (mat))
(defmethod determinant ((mat jg-matrix))
  (let ((det 0))
    (cond ((and (= (rows mat) 2)
                (= (columns mat) 2))
           (- (* (element-at mat 0 0)
                 (element-at mat 1 1))
              (* (element-at mat 0 1)
                 (element-at mat 1 0))))
          (t (dotimes (i (columns mat) det)
               (setf det (+ det 
                            (* (element-at mat 0 i)
                               (cofactor mat 0 i)))))))))

(defgeneric submatrix (mat row column))
(defmethod submatrix ((mat jg-matrix) (row fixnum) (column fixnum))
  (let ((result (make-list 0)))
    (dotimes (i (rows mat))
      (if (not (= i row))
          (dotimes (j (columns mat))
            (if (not (= j column))
                (push (element-at mat i j) result)))))
    (make-jg-matrix (- (rows mat) 1)
                    (- (columns mat) 1)
                    :data (reverse result))))

(defgeneric minor (mat row column))
(defmethod minor ((mat jg-matrix) (row fixnum) (column fixnum))
  (determinant (submatrix mat row column)))

(defgeneric cofactor (mat row column))
(defmethod cofactor ((mat jg-matrix) 
                     (row fixnum) 
                     (column fixnum))
  (if (oddp (+ row column))
      (- (minor mat row column))
      (minor mat row column)))

(defun invertible? (mat)
  (cond ((= 0 (determinant mat)) nil)
        (t t)))

(defgeneric inverse (mat))
(defmethod inverse ((mat jg-matrix))
  (let ((det (determinant mat)))
    (if (= 0 det)
        (error "Matrix is not invertible, ~A" mat)
        (let ((result (make-jg-matrix (rows mat)
                                      (columns mat))))
          (dotimes (i (rows result) result)
            (dotimes (j (columns result) result)
              (setf (element-at result j i)
                    (/ (cofactor mat i j) det))))))))
