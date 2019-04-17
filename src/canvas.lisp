(in-package :jg-cl-canvas)
(defclass jg-canvas ()
  ((width :initarg :width
          :reader width)
   (height :initarg :height
           :reader height)
   (pixels :initarg :pixels
           :reader pixels)))

(defun make-jg-canvas (width height)
  (let ((wdth (coerce width 'fixnum))
        (ht (coerce height 'fixnum)))
    (make-instance 'jg-canvas 
                   :width wdth
                   :height ht
                   :pixels (make-array (* 3 wdth ht) 
                                       :initial-element 0.0
                                       :element-type 'float))))

(defun jg-canvas? (obj)
  (typep obj 'jg-canvas))

(defgeneric canvas->ppm (cnvs))
(defmethod canvas->ppm ((cnvs jg-canvas))
  (let ((ppm-stream (make-string-output-stream)))
    (format ppm-stream "P3~%")
    (format ppm-stream "~A ~A~%" (width cnvs) (height cnvs))
    (write-string "255" ppm-stream)
    (get-output-stream-string ppm-stream)))

(defgeneric set-color (canvas color x y))
(defmethod set-color ((canvas jg-canvas) 
                      (color jg-color) 
                      (x fixnum) 
                      (y fixnum))
  (let ((index (+ (* 3 x) (* y (width canvas))))
        (pxls (pixels canvas)))
    (setf (elt pxls index) (r color))
    (setf (elt pxls (+ index 1)) (g color))
    (setf (elt pxls (+ index 2)) (b color))))

(defgeneric get-color (canvas x y))
(defmethod get-color ((canvas jg-canvas) 
                      (x fixnum) 
                      (y fixnum))
  (let ((index (+ (* 3 x) (* y (width canvas))))
        (pxls (pixels canvas)))
    (make-jg-color (elt pxls index)
                   (elt pxls (+ index 1))
                   (elt pxls (+ index 2)))))
