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
