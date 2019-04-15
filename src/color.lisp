(in-package :jg-cl-color)
(defclass jg-color ()
  ((r :initarg :r
      :reader r)
   (g :initarg :g
      :reader g)
   (b :initarg :b
      :reader b)))
(defun make-jg-color (r g b)
  (make-instance 'jg-color
                 :r (coerce r 'float)
                 :g (coerce g 'float)
                 :b (coerce b 'float)))

(defun jg-color? (obj)
    (typep obj 'jg-color))

(defmethod equivalent ((obj1 jg-color) (obj2 jg-color))
  (and (equivalent (r obj1) (r obj2))
       (equivalent (g obj1) (g obj2))
       (equivalent (b obj1) (b obj2))))

(defmethod add ((addend1 jg-color) (addend2 jg-color))
  (make-jg-color (+ (r addend1) (r addend2))
                 (+ (g addend1) (g addend2))
                 (+ (b addend1) (b addend2))))

(defmethod subtract ((subend1 jg-color) (subend2 jg-color))
  (make-jg-color (+ (r subend1) (r subend2))
                 (+ (g subend1) (g subend2))
                 (+ (b subend1) (b subend2))))

(defmethod scale ((color jg-color) (scalar float))
  (make-jg-color (* (r color) scalar)
                 (* (g color) scalar)
                 (* (b color) scalar)))
