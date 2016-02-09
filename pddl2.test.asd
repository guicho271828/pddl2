#|
  This file is a part of pddl2 project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage pddl2.test-asd
  (:use :cl :asdf))
(in-package :pddl2.test-asd)


(defsystem pddl2.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of pddl2"
  :license "LLGPL"
  :depends-on (:pddl2
               :fiveam
               :split-sequence)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c)
                    (let ((*default-pathname-defaults* (asdf:system-source-directory :pddl2)))
                      (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :pddl2))")))))
