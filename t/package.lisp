#|
  This file is a part of pddl2 project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :pddl2.test
  (:use :cl
        :pddl2
        :fiveam
        :trivia :alexandria :iterate))
(in-package :pddl2.test)



(def-suite :pddl2)
(in-suite :pddl2)

;; run test with (run! test-name) 

(test pddl2

  )



