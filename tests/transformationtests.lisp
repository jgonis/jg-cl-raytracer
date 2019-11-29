(in-package :jg-cl-raytracer.test)
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

(test shearing-x-around-y
  (let* ((shear (make-shearing-matrix 1 0 0 0 0 0))
         (p (make-jg-point 2 3 4))
         (expected (make-jg-point 5 3 4))
         (result (multiply shear p)))
    (is (equivalent expected result))))

(test shearing-x-around-z
  (let* ((shear (make-shearing-matrix 0 1  0 0 0 0))
         (p (make-jg-point 2 3 4))
         (expected (make-jg-point 6 3 4))
         (result (multiply shear p)))
    (is (equivalent expected result))))

(test shearing-y-around-x
(let* ((shear (make-shearing-matrix 0 0 1 0 0 0))
         (p (make-jg-point 2 3 4))
         (expected (make-jg-point 2 5 4))
         (result (multiply shear p)))
    (is (equivalent expected result))))

(test shearing-y-around-z
  (let* ((shear (make-shearing-matrix 0 0 0 1 0 0))
         (p (make-jg-point 2 3 4))
         (expected (make-jg-point 2 7 4))
         (result (multiply shear p)))
    (is (equivalent expected result))))

(test shearing-z-around-x
  (let* ((shear (make-shearing-matrix 0 0 0 0 1 0))
         (p (make-jg-point 2 3 4))
         (expected (make-jg-point 2 3 6))
         (result (multiply shear p)))
    (is (equivalent expected result))))

(test shearing-z-around-y
 (let* ((shear (make-shearing-matrix 0 0 0 0 0 1))
         (p (make-jg-point 2 3 4))
         (expected (make-jg-point 2 3 7))
         (result (multiply shear p)))
    (is (equivalent expected result))))

(test combine-transforms
  (let* ((pt (make-jg-point 1 0 1))
         (A (make-x-rotation-matrix (/ PI 2)))
         (B (make-scaling-matrix 5 5 5))
         (C (make-translation-matrix 10 5 7))
         (expected-pt2 (make-jg-point 1 -1 0))
         (pt2 (multiply A pt))
         (expected-pt3 (make-jg-point 5 -5 0))
         (pt3 (multiply B pt2))
         (expected-pt4 (make-jg-point 15 0 7))
         (pt4 (multiply C pt3)))
    (is (equivalent expected-pt2 pt2))
    (is (equivalent expected-pt3 pt3))
    (is (equivalent expected-pt4 pt4))))

(test combined-transformations
  (let* ((p (make-jg-point 1 0 1))
         (A (make-x-rotation-matrix (/ PI 2)))
         (B (make-scaling-matrix 5 5 5))
         (C (make-translation-matrix 10 5 7))
         (combined-trans (multiply C (multiply B A)))
         (expected (make-jg-point 15 0 7))
         (result (multiply combined-trans p)))
    (is (equivalent expected result))))

(test rotate-method-on-matrices
  (let* ((p (make-jg-point 0 1 0))
         (half-quarter (rotate-x (make-identity-matrix) 
                                 (/ PI 4)))
         (full-quarter (rotate-x (make-identity-matrix)
                                 (/ PI 2)))
         (expected-half (make-jg-point 0 
                                       (/ (sqrt 2) 2)
                                       (/ (sqrt 2) 2)))
         (expected-full (make-jg-point 0 0 1))
         (result-half (multiply half-quarter p))
         (result-full (multiply full-quarter p)))
    (is (equivalent expected-half result-half))
    (is (equivalent expected-full result-full))))

(test combined-transformations-without-multiply
  (let* ((p (make-jg-point 1 0 1))
         (transform (translate 
                     (scale
                      (rotate-x (make-identity-matrix) 
                              (/ PI 2))
                      5 5 5)
                     10 5 7))
         (expected (make-jg-point 15 0 7))
         (result (multiply transform p)))
    (is (equivalent expected result))))
