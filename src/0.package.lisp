#|
  This file is a part of pddl2 project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :pddl2.impl
  (:use :cl :trivia :alexandria :iterate
        :lisp-namespace)
  (:export :read-pddl))

