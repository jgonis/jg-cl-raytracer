(in-package :jg-cl-raytracer.test)
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
         (result (uniform-scale v1 3.5))
         (expected-result-vec (make-jg-vec 3.5 -7 10.5)))
    (is (jg-vec? result))
    (is (equivalent expected-result-vec result))))

(test scale-vector-by-fraction
  (let* ((v1 (make-jg-vec 1 -2 3))
         (result (uniform-scale v1 0.5))
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
