(in-package :jg-cl-raytracer.test)
(defun run-tests ()
  (1am:run))

(test point-creation
  (let ((pt (make-jg-point 4.3 3.1 1.0)))
    (is (= (x pt) 4.3))
    (is (= (y pt) 3.1))
    (is (= (z pt) 1.0))
    (is (jg-pointp pt))
    (is (null (jg-vecp pt)))))
