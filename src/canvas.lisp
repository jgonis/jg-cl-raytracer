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
                                       :initial-element 0))))

(defun jg-canvas? (obj)
  (typep obj 'jg-canvas))

(defgeneric set-color (canvas color x y))
(defmethod set-color ((canvas jg-canvas) 
                      (color jg-color) 
                      (x fixnum) 
                      (y fixnum)))

(defgeneric get-color (canvas x y))
(defun get-color ((canvas jg-canvas) 
                  (x fixnum) 
                  (y fixnum)))
