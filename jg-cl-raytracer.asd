(defsystem :jg-cl-raytracer
  :description "Work on making a raytracer in Common Lisp"
  :version "0.0.1"
  :depends-on (:zpng)
  :author "Jeff Gonis <jeffgonis@fastmail.com"
  :licence "LGPL 3.0"
  :components ((:file "packages")
               (:module "src" 
                :serial t 
                :components ((:file "main")))))

(defsystem :jg-cl-raytracer/test
  :description "Test suite for the raytracer"
  :license "LGPL 3.0"
  :author "Jeff Gonis"
  :depends-on (:jg-cl-raytracer :1am)
  :serial t
  :components ((:file "packages.test")
               (:module "tests"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (op system)
             (funcall (read-from-string "jg-cl-raytracer.test:run-tests"))))