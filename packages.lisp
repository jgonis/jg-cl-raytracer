(declaim (optimize (debug 3)))
(setf *read-default-float-format* 'double-float)

(defpackage :jg-cl-utils
  (:nicknames :jgutl)
  (:use :cl)
  (:export :equivalent
           :add
           :subtract
           :multiply
           :uniform-scale
           :make-growable-vector
           :read-lines-to-sequence
           :wrap-lines-longer-than
           :output-canvas-to-file))

(defpackage :jg-cl-color
  (:nicknames :jgclr)
  (:use :cl
        :jgutl)
  (:export :jg-color
           :make-jg-color
           :jg-color?
           :r
           :g
           :b))

(defpackage :jg-cl-geom
  (:nicknames :jggeom)
  (:use :cl
        :jgutl)
  (:export :make-jg-vec 
           :make-jg-point
           :make-jg-matrix
           :make-identity-matrix
           :make-translation-matrix
           :make-scaling-matrix
           :make-x-rotation-matrix
           :make-y-rotation-matrix
           :make-z-rotation-matrix
           :make-shearing-matrix
           :element-at
           :x
           :y
           :z
           :rows
           :columns
           :transpose
           :negate
           :div
           :magnitude
           :normalize
           :dot-product
           :cross-prod
           :determinant
           :submatrix
           :minor
           :cofactor
           :inverse
           :rotate-x
           :rotate-y
           :rotate-z
           :scale
           :translate
           :jg-point?
           :jg-vec?
           :jg-matrix?
           :invertible?))

(defpackage :jg-cl-canvas
  (:nicknames :jgcnvs)
  (:use :cl
        :jg-cl-color)
  (:export :make-jg-canvas
           :jg-canvas?
           :canvas->ppm
           :width
           :height
           :set-color
           :get-color))
