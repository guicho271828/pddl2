#|
  This file is a part of pddl2 project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Simpler PDDL (Planning Domain Description Language) parser in CL

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage pddl2-asd
  (:use :cl :asdf))
(in-package :pddl2-asd)


(defsystem pddl2
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :alexandria :iterate)
  :components ((:module "src"
                :components
                ((:file "0.package"))))
  :description "Simpler PDDL (Planning Domain Description Language) parser in CL"
  :in-order-to ((test-op (load-op :pddl2.test))))
