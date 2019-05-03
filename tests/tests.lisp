(in-package :jg-cl-raytracer.test)
(defun run-tests ()
  (1am:run))

(test point-creation
  (let ((pt (make-jg-point 4.3 -4.2 3.1)))
    (is (= (x pt) 4.3))
    (is (= (y pt) -4.2))
    (is (= (z pt) 3.1))
    (is (jg-point? pt))
    (is (null (jg-vec? pt)))))

(test vec-creation
  (let ((vec (make-jg-vec 4.3 -4.2 3.1)))
    (is (= (x vec) 4.3))
    (is (= (y vec) -4.2))
    (is (= (z vec) 3.1))
    (is (null (jg-point? vec)))
    (is (jg-vec? vec))))

(test add-vec-to-point
  (let* ((p1 (make-jg-point 3 -2 5))
        (v1 (make-jg-vec -2 3 1))
        (p2 (add p1 v1)))
    (is (jg-point? p2))
    (is (null (jg-vec? p2)))
    (is (= 1 (x p2)))
    (is (= 1 (y p2)))
    (is (= 6 (z p2)))))

(test subtract-two-points
  (let* ((p1 (make-jg-point 3 2 1))
         (p2 (make-jg-point 5 6 7))
         (v1 (subtract p1 p2)))
    (is (jg-vec? v1))
    (is (null (jg-point? v1)))
    (is (= -2 (x v1)))
    (is (= -4 (y v1)))
    (is (= -6 (z v1)))))

(test subtract-vec-from-point
  (let* ((p1 (make-jg-point 3 2 1))
         (v1 (make-jg-vec 5 6 7))
         (p2 (subtract p1 v1)))
    (is (jg-point? p2))
    (is (null (jg-vec? p2)))
    (is (= -2 (x p2)))
    (is (= -4 (y p2)))
    (is (= -6 (z p2)))))

(test subtract-vec-from-vec
  (let* ((v1 (make-jg-vec 3 2 1))
         (v2 (make-jg-vec 5 6 7))
         (v3 (subtract v1 v2)))
    (is (jg-vec? v3))
    (is (= -2 (x v3)))
    (is (= -4 (y v3)))
    (is (= -6 (z v3)))))

(test negate-vec
  (let* ((v1 (make-jg-vec 1 -2 3))
         (v2 (negate v1)))
    (is (jg-vec? v2))
    (is (= -1 (x v2)))
    (is (= 2 (y v2)))
    (is (= -3 (z v2)))))

(test scale-vector-by-number
  (let* ((v1 (make-jg-vec 1 -2 3))
         (result (scale v1 3.5))
         (expected-result-vec (make-jg-vec 3.5 -7 10.5)))
    (is (jg-vec? result))
    (is (equivalent expected-result-vec result))))

(test scale-vector-by-fraction
  (let* ((v1 (make-jg-vec 1 -2 3))
         (result (scale v1 0.5))
         (expected-result-vec (make-jg-vec 0.5 -1 1.5)))
    (is (jg-vec? result))
    (is (equivalent expected-result-vec result))))

(test divide-vector
  (let* ((v1 (make-jg-vec 1 -2 3))
         (result (div v1 2))
         (expected-result-vec (make-jg-vec 0.5 -1 1.5)))
    (is (jg-vec? result))
    (is (equivalent expected-result-vec result))))

(test magnitude-x-unit-vector
  (let* ((v1 (make-jg-vec 1 0 0))
         (mag (magnitude v1)))
    (is (equivalent mag 1.0))))

(test magnitude-y-unit-vector
  (let* ((v1 (make-jg-vec 0 1 0))
         (mag (magnitude v1)))
    (is (equivalent mag 1.0))))

(test magnitude-z-unit-vector
  (let* ((v1 (make-jg-vec 0 0 1))
         (mag (magnitude v1)))
    (is (equivalent mag 1.0))))

(test magnitude-positive-vector
  (let* ((v1 (make-jg-vec 1 2 3))
         (mag (magnitude v1))
         (expected-magnitude (sqrt 14)))
    (is (equivalent expected-magnitude mag))))

(test magnitude-negative-vector
  (let* ((v1 (make-jg-vec -1 -2 -3))
         (mag (magnitude v1))
         (expected-magnitude (sqrt 14)))
    (is (equivalent expected-magnitude mag))))

(test vector-normalize-x-vector
  (let* ((v1 (make-jg-vec 4 0 0))
         (result (normalize v1))
         (expected-result-vec (make-jg-vec 1 0 0)))
    (is (jg-vec? result))
    (is (equivalent expected-result-vec result))))

(test vector-normalize
  (let* ((v1 (make-jg-vec 1 2 3))
         (result (normalize v1))
         (mag (sqrt (+ (expt 1 2) 
                       (expt 2 2) 
                       (expt 3 2))))
         (expected-result-vec (make-jg-vec (/ 1 mag)
                                           (/ 2 mag)
                                           (/ 3 mag))))
    (is (jg-vec? result))
    (is (equivalent expected-result-vec result))))

(test test-dot-product
  (let* ((v1 (make-jg-vec 1 2 3))
         (v2 (make-jg-vec 2 3 4))
         (result (dot-product v1 v2)))

    (is (equivalent 20.0 result))))

(test test-cross-product
  (let* ((v1 (make-jg-vec 1 2 3))
         (v2 (make-jg-vec 2 3 4))
         (result (cross-prod v1 v2))
         (expected-result-vec (make-jg-vec -1 2 -1)))
    (is (jg-vec? result))
    (is (equivalent expected-result-vec result))))

(test test-cross-product-2
  (let* ((v1 (make-jg-vec 1 2 3))
         (v2 (make-jg-vec 2 3 4))
         (result (cross-prod v2 v1))
         (expected-result-vec (make-jg-vec 1 -2 1)))
    (is (jg-vec? result))
    (is (equivalent expected-result-vec result))))

(test make-color-test
  (let* ((color (make-jg-color -0.5 0.4 1.7)))
    (is (equivalent -0.5 (r color)))
    (is (equivalent 0.4 (g color)))
    (is (equivalent 1.7 (b color)))))

(test add-colors
  (let* ((c1 (make-jg-color 0.9 0.6 0.75))
         (c2 (make-jg-color 0.7 0.1 0.25))
         (expected-result (make-jg-color 1.6 0.7 1.0))
         (result (add c1 c2)))
    (is (jg-color? result))
    (is (equivalent expected-result result))))

(test subtract-colors
  (let* ((c1 (make-jg-color 0.9 0.6 0.75))
         (c2 (make-jg-color 0.7 0.1 0.25))
         (expected-result (make-jg-color 0.2 0.5 0.5))
         (result (subtract c1 c2)))
    (is (jg-color? result))
    (is (equivalent expected-result result))))

(test scale-color
  (let* ((c1 (make-jg-color 0.2 0.3 0.4))
         (expected-result (make-jg-color 0.4 0.6 0.8))
         (result (scale c1 2.0)))
    (is (jg-color? result))
    (is (equivalent expected-result result))))

(test multiply-color
  (let* ((c1 (make-jg-color 1 0.2 0.4))
         (c2 (make-jg-color 0.9 1 0.1))
         (expected-result (make-jg-color 0.9 0.2 0.04))
         (result (multiply c1 c2)))
    (is (jg-color? result))
    (is (equivalent expected-result result))))

(test create-canvas
  (let* ((cnvs (make-jg-canvas 10 20))
         (black (make-jg-color 0 0 0)))
    (is (jg-canvas? cnvs))
    (is (= 10 (width cnvs)))
    (is (= 20 (height cnvs)))
    (dotimes (y (height cnvs))
      (dotimes (x (width cnvs))
        (let ((clr (get-color cnvs x y)))
          (is (equivalent black clr)))))))

(test get-and-set-color
  (let* ((cnvs (make-jg-canvas 10 20))
         (red (make-jg-color 1 0 0)))
    (set-color cnvs red 2 3)
    (is (equivalent red (get-color cnvs 2 3)))))

(test canvas-to-ppm-header
  (let* ((cnvs (make-jg-canvas 5 3))
         (lines (read-lines-to-sequence (canvas->ppm cnvs)))
         (line1 "P3")
         (line2 "5 3")
         (line3 "255"))
    (is (string= line1 (elt lines 0)))
    (is (string= line2 (elt lines 1)))
    (is (string= line3 (elt lines 2)))))

(test canvas-to-ppm-data
  (let ((expected-lines (make-growable-vector 0)))
    (vector-push-extend "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
                        expected-lines)
    (vector-push-extend "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
                        expected-lines)
    (vector-push-extend "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
                        expected-lines)
    (let* ((cnvs (make-jg-canvas 5 3))
           (c1 (make-jg-color 1.5 0 0))
           (c2 (make-jg-color 0 0.5 0))
           (c3 (make-jg-color -0.5 0 1)))
      (set-color cnvs c1 0 0)
      (set-color cnvs c2 2 1)
      (set-color cnvs c3 4 2)
      (let* ((result (canvas->ppm cnvs))
             (result-lines (read-lines-to-sequence result)))
        (is (string= (elt result-lines 3) 
                     (elt expected-lines 0)))
        (is (string= (elt result-lines 4)
                     (elt expected-lines 1)))
        (is (string= (elt result-lines 5)
                     (elt expected-lines 2)))))))

(test canvas-to-ppm-line-wrapping
  (let* ((cnvs (make-jg-canvas 10 2))
         (clr (make-jg-color 1 0.8 0.6)))
    (dotimes (x (width cnvs)) 
      (dotimes (y (height cnvs))
        (set-color cnvs clr x y)))
    (let* ((result (canvas->ppm cnvs))
           (result-lines (read-lines-to-sequence result))
           (expected-line1 
            "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204")
           (expected-line2 
            "153 255 204 153 255 204 153 255 204 153 255 204 153"))
      (is (string= expected-line1 (elt result-lines 3)))
      (is (string= expected-line2 (elt result-lines 4)))
      (is (string= expected-line1 (elt result-lines 5)))
      (is (string= expected-line2 (elt result-lines 6))))))

(test matrix-equality-is-true
  (let ((m1 (make-jg-matrix 3 
                            3 
                            :data '(1 2 3 4 5 6 7 8 9)))
        (m2 (make-jg-matrix 3
                            3
                            :data '(1 2 3 4 5 6 7 8 9))))
    (is (equivalent m1 m2))))

(test matrix-equality-is-false
  (let ((m1 (make-jg-matrix 3 
                            3
                            :data '(1 2 3 4 5 6 7 8 9)))
        (m2 (make-jg-matrix 3
                            3
                            :data '(2 3 4 5 6 7 8 9 8))))
    (is (not (equivalent m1 m2)))))

(test matrix-multiply
  (let* ((A (make-jg-matrix 4
                            4
                            :data '(1 2 3 4 5 6 7 8 9 8 7 
                                    6 5 4 3 2)))
         (B (make-jg-matrix 4
                            4
                            :data '(-2 1 2 3 3 2 1 -1 4 3 
                                    6 5 1 2 7 8)))
         (expected (make-jg-matrix 4 
                                   4
                                   :data '(20 22 50 48 44 54 
                                           114 108 40 58 110 
                                           102 16 26 46 42)))
         (result (multiply A B)))
    (is (equivalent expected result))))

(test matrix-multiply-by-tuple
  (let* ((A (make-jg-matrix 4 
                            4 
                            :data '(1 2 3 4 2 4 4 2 8 6 4 1 0 0 0 1)))
         (B (make-jg-matrix 4 1 :data '(1 2 3 1)))
         (expected-result (make-jg-matrix 4 1 :data '(18 24 33 1)))
         (result (multiply A B)))
    (is (equivalent expected-result result))))

(test multiply-by-identity-matrix
  (let* ((A (make-jg-matrix 4
                            4
                            :data '(0 1 2 4 1 2 4 8 2 4 6 
                                    16 4 8 16 32)))
         (B (make-identity-matrix 4 4))
         (result (multiply A B)))
    (is (equivalent A result))))

(test matrix-transpose
  (let* ((A (make-jg-matrix 4 
                            4 
                            :data '(0 9 3 0 9 8 0 8 1 
                                    8 5 3 0 0 5 8)))
         (expected (make-jg-matrix 4
                                   4
                                   :data '(0 9 1 0 9 8 8 0 
                                           3 0 5 5 0 8 3 8)))
         (result (transpose A)))
    (is (equivalent expected result))))

(test transpose-identity
  (let* ((A (make-identity-matrix 4 4))
         (expected (make-identity-matrix 4 4))
         (result (transpose A)))
    (is (equivalent expected result))))

(test calculate-determinant
  (let* ((A (make-jg-matrix 2 2 :data '(1 5 -3 2)))
         (result (determinant A)))
    (is (= 17 result))))

(test submatrix-of-3x3-matrix
  (let* ((A (make-jg-matrix 3
                            3
                            :data '(1 5 0 -3 2 7 0 6 -3)))
         (expected-result (make-jg-matrix 2 
                                          2
                                          :data '(-3 2 0 6)))
         (result (submatrix A 0 2)))
    (is (equivalent expected-result result))))

(test submatrix-of-4x4-matrix
  (let* ((A (make-jg-matrix 4
                            4
                            :data '(-6 1 1 6 -8 5 8 6 -1 
                                    0 8 2 -7 1 -1 1)))
         (expected (make-jg-matrix 3
                                   3
                                   :data '(-6 1 6 -8 8 6 -7 -1 1)))
         (result (submatrix A 2 1)))
    (is (equivalent expected result))))

(test test-minor-of-3x3-matrix
  (let* ((A (make-jg-matrix 3
                            3
                            :data '(3 5 0 2 -1 -7 6 -1 5)))
         (B (submatrix A 1 0))
         (det-b (determinant B))
         (result (minor A 1 0)))
    (is (= det-b result))))

(test test-cofactor-of-3x3-matrix
  (let* ((A (make-jg-matrix 3
                            3
                            :data '(3 5 0 2 -1 -7 6 -1 5)))
         (minor-a (minor A 0 0))
         (cofactor-a (cofactor A 0 0))
         (minor-a-2 (minor A 1 0))
         (cofactor-a-2 (cofactor A 1 0)))
    (is (= minor-a -12))
    (is (= cofactor-a -12))
    (is (= minor-a-2 25))
    (is (= cofactor-a-2 -25))))

(test determinant-of-3x3-matrix
  (let ((A (make-jg-matrix 3
                            3
                            :data'(1 2 6 -5 8 -4 2 6 4))))
    (is (= (cofactor A 0 0) 56))
    (is (= (cofactor A 0 1) 12))
    (is (= (cofactor A 0 2) -46))
    (is (= (determinant A) -196))))

(test determinant-of-4x4-matrix
  (let ((A (make-jg-matrix 4
                           4
                           :data '(-2 -8 3 5 -3 1 7 3 1 
                                   2 -9 6 -6 7 7 -9))))
    (is (= (cofactor A 0 0) 690))
    (is (= (cofactor A 0 1) 447))
    (is (= (cofactor A 0 2) 210))
    (is (= (cofactor A 0 3) 51))
    (is (= (determinant A) -4071))))
