(in-package :jg-cl-raytracer.test)
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
         (result (uniform-scale c1 2.0)))
    (is (jg-color? result))
    (is (equivalent expected-result result))))

(test multiply-color
  (let* ((c1 (make-jg-color 1 0.2 0.4))
         (c2 (make-jg-color 0.9 1 0.1))
         (expected-result (make-jg-color 0.9 0.2 0.04))
         (result (multiply c1 c2)))
    (is (jg-color? result))
    (is (equivalent expected-result result))))
