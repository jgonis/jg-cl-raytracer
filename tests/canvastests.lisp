(in-package :jg-cl-raytracer.test)
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
