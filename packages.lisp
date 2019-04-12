(defpackage :jg-cl-raytracer
  (:nicknames :jgrt)
  (:use :cl)
  (:export :make-jg-vec 
           :make-jg-point
           :equivalent
           :add
           :subtract
           :negate
           :scale
           :div
           :magnitude
           :normalize
           :dot-product
           :cross-prod
           :x
           :y
           :z
           :jg-point?
           :jg-vec?))

