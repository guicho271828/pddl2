#|
  This file is a part of pddl2 project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :pddl2.impl
  (:use :cl :trivia :alexandria :iterate
        :lisp-namespace
        :function-cache)
  (:export :read-pddl
           :define
           :domain
           :problem
           :object
           :UNBOUND-PROBLEM
           :UNBOUND-DOMAIN
           ;; Do not export; annoying conflict with iterate. Compare with string=
           ;; :minimize
           ;; :maximize
           :total-cost
           :SYMBOL-DOMAIN
           :DOMAIN-BOUNDP
           :SYMBOL-PROBLEM
           :PROBLEM-BOUNDP
           :*requirements*
           :*types*
           :*objects*
           :*predicates*
           :*numeric-fluents*
           :*object-fluents*
           :*axioms*
           :*actions*
           :*domain*
           :*domain-name*
           :*init*
           :*goal*
           :*metric*
           :*ground-numeric-fluents*
           :*ground-object-fluents*
           :flatten-action
           :-
           :*DOMAIN-TABLE*
           :*PROBLEM-TABLE*
           :ground-problem
           :make-trie
           :merge-trie
           :trie-member
           :lift-or
           :variablep
           :map-trie
           :pop-trie
           :push-trie
           :trie-equal
           :trie-subsetp))

