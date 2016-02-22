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
  :depends-on (:trivia :alexandria :iterate :lisp-namespace :function-cache)
  :components ((:module "src"
                :components
                ((:file "0.package")
                 (:file "1.utility")
                 (:file "1.trie")
                 (:file "1.lift-or")
                 (:file "1.read-file")
                 (:file "2.domain")
                 (:file "2.problem")
                 (:file "3.grounding"))))
  :description "Simpler PDDL (Planning Domain Description Language) parser in CL"
  :in-order-to ((test-op (test-op :pddl2.test))))
