(in-package :jg-cl-canvas)
(defclass jg-canvas ()
  ((width :initarg :width
          :reader width)
   (height :initarg :height
           :reader height)
   (pixels :initarg :pixels
           :reader pixels)))

(defparameter *elements-per-pixel* 3)

(defun make-jg-canvas (width height)
  (let ((wdth (coerce width 'fixnum))
        (ht (coerce height 'fixnum)))
    (make-instance 'jg-canvas 
                   :width wdth
                   :height ht
                   :pixels (make-array (* *elements-per-pixel* 
                                          wdth 
                                          ht) 
                                       :initial-element 0.0
                                       :element-type 'float))))

(defun jg-canvas? (obj)
  (typep obj 'jg-canvas))

(defgeneric canvas->ppm (cnvs))
(defmethod canvas->ppm ((cnvs jg-canvas))
  (let ((pxls (pixels cnvs)) 
        (max-value 255)
        (row-length (* *elements-per-pixel* (width cnvs)))
        (ppm-stream (make-string-output-stream)))
    (format ppm-stream "P3~%")
    (format ppm-stream "~A ~A~%" (width cnvs) (height cnvs))
    (format ppm-stream "~A~%" max-value)
    (dotimes (i (height cnvs))
      (let* ((offset (* i row-length))
             (row (subseq pxls offset (+ row-length offset))))
        (format ppm-stream 
                "~A~%" 
                (jgutl:wrap-lines-longer-than 
                 (pixel-row->string row max-value)
                 70))))
    (format ppm-stream "~%")
    (get-output-stream-string ppm-stream)))

(defun pixel-row->string (row max-value)
  (let* ((scaled-row (map (type-of row) 
                          (lambda (elem) 
                            (cond ((< elem 0) 0)
                                  ((> elem 1) max-value)
                                  (t (round (* elem max-value)))))
                          row))
         (row-stream (make-string-output-stream))
         (element-count (length scaled-row)))
    (dotimes (i element-count)
      (if (= i (- element-count 1))
          (format row-stream "~A" (elt scaled-row i))
          (format row-stream "~A " (elt scaled-row i))))
    (get-output-stream-string row-stream)))

(defgeneric set-color (canvas color x y))
(defmethod set-color ((canvas jg-canvas) 
                      (color jg-color) 
                      x 
                      y)
  (let ((index (coords-to-index x y (width canvas)))
        (pxls (pixels canvas)))
    (setf (elt pxls index) (r color))
    (setf (elt pxls (+ index 1)) (g color))
    (setf (elt pxls (+ index 2)) (b color))))

(defgeneric get-color (canvas x y))
(defmethod get-color ((canvas jg-canvas) 
                      (x fixnum) 
                      (y fixnum))
  (let ((index (coords-to-index x y (width canvas)))
        (pxls (pixels canvas)))
    (make-jg-color (elt pxls index)
                   (elt pxls (+ index 1))
                   (elt pxls (+ index 2)))))

(defun coords-to-index (x y canvas-width)
  (let ((stride (* canvas-width *elements-per-pixel*)))
    (+ (* x *elements-per-pixel*) 
       (* y stride))))
