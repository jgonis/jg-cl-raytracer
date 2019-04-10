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
