(in-package :jg-cl-raytracer.test)
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
