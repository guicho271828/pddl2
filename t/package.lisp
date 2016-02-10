#|
  This file is a part of pddl2 project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :pddl2.test
  (:use :cl
        :pddl2.impl
        :split-sequence
        :fiveam
        :trivia :alexandria :iterate))
(in-package :pddl2.test)



(def-suite :pddl2)
(in-suite :pddl2)

;; run test with (run! test-name) 

(test domains
  (for-all ((path (let ((domains (split-sequence
                                   #\Newline
                                   (uiop:run-program (format nil "find ~a -name '*domain*'"
                                                             (merge-pathnames "t/classical/"
                                                                              *default-pathname-defaults*))
                                                     :output '(:string :stripped t)))))
                     (lambda ()
                       (random-elt domains)))))
    (finishes
      (read-pddl path))))

(test problems
  (for-all ((path (let ((domains (split-sequence
                                   #\Newline
                                   (uiop:run-program (format nil "find ~a -name '*.pddl' | grep -v domain"
                                                              (merge-pathnames "t/classical/"
                                                                               *default-pathname-defaults*))
                                                     :output '(:string :stripped t)))))
                     (lambda ()
                       (random-elt domains)))))
    (finishes
      (handler-case
          (read-pddl path)
        (unbound-domain ()
          nil)))))



