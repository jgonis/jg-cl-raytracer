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
; (ffffff 1d1d1d F4c500 8A00b2 FA6700 88d2ff d90000 c2ad80 768181
;  00a052 ef8de0 0080d5 ff9072 6750b2 ffb000 a93074 e8ca00 8a2013
;  b9d718 6a3e00 e04200 385000)
  '((1.0 1.0 1.0) 
    (0.11372549019607843 0.11372549019607843 0.11372549019607843) 
    (0.9568627450980393 0.7725490196078432 0.0) 
    (0.5411764705882353 0.0 0.6980392156862745) 
    (0.9803921568627451 0.403921568627451 0.0) 
    (0.5333333333333333 0.8235294117647058 1.0) 
    (0.8509803921568627 0.0 0.0) 
    (0.7607843137254902 0.6784313725490196 0.5019607843137255) 
    (0.4627450980392157 0.5058823529411764 0.5058823529411764) 
    (0.0 0.6274509803921569 0.3215686274509804) 
    (0.9372549019607843 0.5529411764705883 0.8784313725490196) 
    (0.0 0.5019607843137255 0.8352941176470589) 
    (1.0 0.5647058823529412 0.4470588235294118) 
    (0.403921568627451 0.3137254901960784 0.6980392156862745) 
    (1.0 0.6901960784313725 0.0) 
    (0.6627450980392157 0.18823529411764706 0.4549019607843137) 
    (0.9098039215686274 0.792156862745098 0.0) 
    (0.5411764705882353 0.12549019607843137 0.07450980392156863) 
    (0.7254901960784313 0.8431372549019608 0.09411764705882353) 
    (0.41568627450980394 0.24313725490196078 0.0) 
    (0.8784313725490196 0.25882352941176473 0.0) 
    (0.2196078431372549 0.3137254901960784 0.0)))
