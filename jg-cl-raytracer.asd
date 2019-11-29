(defsystem :jg-cl-raytracer
  :description "Work on making a raytracer in Common Lisp"
  :version "0.0.1"
  :author "Jeff Gonis <jeffgonis@fastmail.com>"
  :licence "LGPL 3.0"
  :components ((:file "packages")
               (:module "src" 
                        :serial t 
                        :components ((:file "utils") 
                                     (:file "geom")
                                     (:file "color")
                                     (:file "canvas")
                                     (:file "main")))))

(defsystem :jg-cl-raytracer/test
  :description "Test suite for the raytracer"
  :license "LGPL 3.0"
  :author "Jeff Gonis"
  :depends-on (:jg-cl-raytracer 
               :1am
               :cl-utilities)
  :serial t
  :components ((:file "packages.test")
               (:module "tests"
                :serial t
                :components ((:file "tests")
                             (:file "geometrytests")
                             (:file "colortests")
                             (:file "canvastests")
                             (:file "matrixtests")
                             (:file "transformationtests"))))
  :perform (asdf:test-op (op system)
             (funcall (read-from-string "jg-cl-raytracer.test:run-tests"))))
