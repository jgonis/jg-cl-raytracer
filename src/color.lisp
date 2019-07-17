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
  (make-jg-color (- (r subend1) (r subend2))
                 (- (g subend1) (g subend2))
                 (- (b subend1) (b subend2))))

(defmethod uniform-scale ((color jg-color) (scalar float))
  (make-jg-color (* (r color) scalar)
                 (* (g color) scalar)
                 (* (b color) scalar)))

(defmethod multiply ((color1 jg-color) (color2 jg-color))
  (make-jg-color (* (r color1) (r color2))
                 (* (g color1) (g color2))
                 (* (b color1) (b color2))))

(defmethod print-object ((clr jg-color) strm)
  (format strm 
          "Color r:~A g:~A b:~A~%" 
          (r clr)
          (g clr)
          (b clr)))

(defun get-kellys-colors ()  
  '(ffffff
    1d1d1d
    F4c500
    8A00b2
    FA6700
    88d2ff
    d90000
    c2ad80
    768181
    00a052
    ef8de0
    0080d5
    ff9072
    6750b2
    ffb000
    a93074
    e8ca00
    8a2013
    b9d718
    6a3e00
    e04200
    385000))

(defun parse-func (str)
  (let ((vals (list)))
    (do ((i 0 (+ i 2)))
        ((>= i (length str)))
      (setf vals (cons (parse-integer (subseq str i (+ i 2)) 
                                      :radix 16)
                       vals)))
    (nreverse vals)))
