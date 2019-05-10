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

(test is-matrix-invertible
  (let ((A (make-jg-matrix 4
                           4
                           :data '(6 4 4 4 5 5 7 6 4 -9 3 
                                   -7 9 1 7 -6)))
        (B (make-jg-matrix 4 
                           4
                           :data '(-4 2 -2 -3 9 6 2 6 0 -5 
                                   1 -5 0 0 0 0))))
    (is (invertible? A))
    (is (not (invertible? B)))))

(test invert-matrix
  (let* ((A (make-jg-matrix 4
                            4
                            :data '(-5 2 6 -8 1 -5 1 8 7 7 
                                    -6 -7 1 -3 7 4)))
         (l '(116 240 128 -24 -430 -775 -236 277 
              -42 -119 -28 105 -278 -433 -160 163))
         (scaled-list (mapcar (lambda (x) (/ x 532)) l))
         (expected (make-jg-matrix 4
                                   4
                                   :data scaled-list))
         (result (inverse A)))
    (is (= (determinant A) 532.0))
    (is (= (cofactor A 2 3) -160.0))
    (is (equivalent (element-at result 3 2)
                    (/ -160 532.0)))
    (is (= (cofactor A 3 2) 105.0))
    (is (equivalent (element-at result 2 3)
                    (/ 105 532.0)))
    (is (equivalent expected result))))

(test invert-another-matrix
  (let* ((A (make-jg-matrix 4
                           4
                           :data '(8 -5 9 2 7 5 6 1 -6 0 9 
                                   6 -3 0 -9 -4)))
        (l '(-0.153846 -0.153846 -0.2820513 -0.5384615 -0.076923 
             0.123076 0.025641 0.0307692 0.3589743 0.3589743 
             0.4358974  0.9230769 -0.6923077 -0.6923077 -0.7692308 
             -1.9230769))
        (expected (make-jg-matrix 4
                                  4
                                  :data l))
         (result (inverse A)))
    (is (equivalent expected result))))

(test third-invert-test
  (let* ((A (make-jg-matrix 4
                            4
                            :data '(9 3 0 9 -5 -2 -6 -3 -4 9 
                                    6 4 -7 6 6 2)))
         (l '(-0.0407407 -0.0777778 0.14444445 -0.2222222 -0.0777778 
              0.03333333 0.3666667 -0.3333333 -0.0290123 -0.1462963 
              -0.1092593 0.1296296 0.1777778 0.0666667 -0.2666667 
              0.3333333))
         (expected (make-jg-matrix 4
                                   4
                                   :data l))
         (result (inverse A)))
    (is (equivalent expected result))))

(test multiply-matrix-by-inverse
  (let* ((A (make-jg-matrix 4
                            4
                            :data '(3 -9 7 3 3 -8 2 -9 -4 4 
                                    4 1 -6 5 -1 1)))
         (B (make-jg-matrix 4
                            4
                            :data '(8 2 2 2 3 -1 7 0 7 0 5 4 
                                    6 -2 0 5)))
         (C (multiply A B))
         (inv-B (inverse B))
         (result (multiply C inv-B)))
    (is (equivalent A result))))

(test translate-point
  (let* ((trans-matrix (make-translation-matrix 5 -3 2))
         (pt (make-jg-point -3 4 5))
         (expected (make-jg-point 2 1 7))
         (result (multiply trans-matrix pt)))
    (is (equivalent expected result))))

(test translate-vector-has-no-effect
  (let* ((trans-matrix (make-translation-matrix 5 -3 2))
         (vec (make-jg-vec -3 4 5))
         (result (multiply trans-matrix vec)))
    (is (equivalent vec result))))

(test invert-translation-matrix-reverses-translation
  (let* ((trans-matrix (make-translation-matrix 5 -3 2))
         (inv-trans (inverse trans-matrix))
         (p (make-jg-point -3 4 5))
         (expected (make-jg-point -8 7 3))
         (result (multiply inv-trans p)))
    (is (equivalent expected result))))

(test apply-scale-matrix-to-point
  (let* ((scale-mat (make-scaling-matrix 2 3 4))
         (p (make-jg-point -4 6 8))
         (expected (make-jg-point -8 18 32))
         (result (multiply scale-mat p)))
    (is (equivalent expected result))))

(test apply-scale-matrix-to-vec
  (let* ((scale-mat (make-scaling-matrix 2 3 4))
         (v (make-jg-vec -4 6 8))
         (expected (make-jg-vec -8 18 32))
         (result (multiply scale-mat v)))
    (is (equivalent expected result))))

(test apply-inverse-scale-matrix-shrinks
  (let* ((scale-mat (make-scaling-matrix 2 3 4))
         (inv-scale (inverse scale-mat))
         (v (make-jg-vec -4 6 8))
         (expected (make-jg-vec -2 2 2))
         (result (multiply inv-scale v)))
    (is (equivalent expected result))))

(test reflection-is-scaling-negatively
  (let* ((scale-mat (make-scaling-matrix -1 1 1))
         (p (make-jg-point 2 3 4))
         (expected (make-jg-point -2 3 4))
         (result (multiply scale-mat p)))
    (is (equivalent expected result))))

(test rotate-point-around-x-axis
  (let* ((p (make-jg-point 0 1 0))
         (half-quarter (make-x-rotation-matrix (/ PI 4)))
         (full-quarter (make-x-rotation-matrix (/ PI 2)))
         (expected-half (make-jg-point 0 
                                       (/ (sqrt 2) 2)
                                       (/ (sqrt 2) 2)))
         (expected-full (make-jg-point 0 0 1))
         (result-half (multiply half-quarter p))
         (result-full (multiply full-quarter p)))
    (is (equivalent expected-half result-half))
    (is (equivalent expected-full result-full))))

(test inverse-rotation-rotates-opposite
  (let* ((p (make-jg-point 0 1 0))
         (half-quarter (make-x-rotation-matrix (/ PI 4)))
         (inv-rot (inverse half-quarter))
         (expected (make-jg-point 0 
                                  (/ (sqrt 2) 2) 
                                  (/ (- (sqrt 2)) 2)))
         (result (multiply inv-rot p)))
    (is (equivalent expected result))))

(test rotate-point-around-y-axis
  (let* ((p (make-jg-point 0 0 1))
         (half-quarter (make-y-rotation-matrix (/ PI 4)))
         (full-quarter (make-y-rotation-matrix (/ PI 2)))
         (expected-half (make-jg-point (/ (sqrt 2) 2) 
                                       0
                                       (/ (sqrt 2) 2)))
         (expected-full (make-jg-point 1 0 0))
         (result-half (multiply half-quarter p))
         (result-full (multiply full-quarter p)))
    (is (equivalent expected-half result-half))
   (is (equivalent expected-full result-full))))

(test rotate-point-around-z-axis
  (let* ((p (make-jg-point 0 1 0))
         (half-quarter (make-z-rotation-matrix (/ PI 4)))
         (full-quarter (make-z-rotation-matrix (/ PI 2)))
         (expected-half (make-jg-point (- (/ (sqrt 2) 2))
                                       (/ (sqrt 2) 2)
                                       0))
         (expected-full (make-jg-point -1 0 0))
         (result-half (multiply half-quarter p))
         (result-full (multiply full-quarter p)))
    (is (equivalent expected-half result-half))
    (is (equivalent expected-full result-full))))
