(defpackage :jg-cl-utils
  (:nicknames :jgutl)
  (:use :cl)
  (:export :equivalent
           :add
           :subtract
           :scale))

(defpackage :jg-cl-color
  (:nicknames :jgclr)
  (:use :cl
        :jgutl)
  (:export :make-jg-color
           :jg-color?
           :r
           :g
           :b
           :multiply))

(defpackage :jg-cl-geom
  (:nicknames :jggeom)
  (:use :cl
        :jgutl)
  (:export :make-jg-vec 
           :make-jg-point
           :x
           :y
           :z
           :negate
           :div
           :magnitude
           :normalize
           :dot-product
           :cross-prod
           :jg-point?
           :jg-vec?))
(defpackage :jg-cl-canvas
  (:nicknames :jgcnvs)
  (:use :cl
        :jgclr)
  (:export :make-jg-canvas
           :jg-canvas?
           :width
           :height
           :set-color
           :get-color))
