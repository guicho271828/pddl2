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



(test parse-effect
  (signals error
    ;; syntax error in WHEN
    (let ((*types* '((place object) (object)))
          (*objects* '((a . place) (b . place) (c . object))))
      (print
       (flatten-action
        '(move :parameters (?a ?b - place ?c)
          :precondition (and (at ?c ?a) (forall (?d - place) (foo ?d)))
          :effect (and
                   (not (at ?c ?a))
                   (at ?c ?b)
                   (when (bar ?c)
                     (not (bar ?c))
                     (when (baz ?c)
                       (not (baz ?c))))
                   (forall (?d - place) (not (foo ?d)))))))))
  (signals error
    ;; nested WHEN
    (let ((*types* '((place object) (object)))
          (*objects* '((a . place) (b . place) (c . object))))
      (print
       (flatten-action
        '(move :parameters (?a ?b - place ?c)
          :precondition (and (at ?c ?a) (forall (?d - place) (foo ?d)))
          :effect (and
                   (not (at ?c ?a))
                   (at ?c ?b)
                   (when (bar ?c)
                     (and (not (bar ?c))
                          (when (baz ?c)
                            (not (baz ?c)))))
                   (forall (?d - place) (not (foo ?d))))))))))

(test trie
  (trace make-trie trie= trie-member)
  (finishes (make-trie '()))
  (finishes
    (make-trie '((a x y)
                 (b x y z)
                 (a x z)
                 (b x z y))))
  (finishes (make-trie '((a) (b))))
  (finishes (make-trie '((a) (a))))
  (finishes
    (merge-trie (print (make-trie '((a x y))))
                (print (make-trie '((a x z))))))
  (is-true
   (trie= (make-trie '())
          (make-trie '())))
  (is-true
   (trie= (make-trie '((a x y)))
          (make-trie '((a x y)))))
  (is-true
   (trie= (make-trie '((a x y)
                       (a x z)))
          (make-trie '((a x z)
                       (a x y)))))
  (is-true
   (trie= (make-trie '((a x y)
                       (c a x y)
                       (a x z)))
          (make-trie '((a x z)
                       (a x y)
                       (c a x y)))))
  (is-true
   (trie-member
    '(c a x y)
    (make-trie '((a x y)
                 (c a x y)
                 (a x z)))))
  (is-false
   (trie-member
    '(c a x z)
    (make-trie '((a x y)
                 (c a x y)
                 (a x z)))))
  (is-false
   (trie-member
    nil
    (make-trie '((a x y)
                 (c a x y)
                 (a x z)))))
  (is-false
   (trie-member
    '(b)
    (make-trie '((a x y)
                 (c a x y)
                 (a x z)))))
  (is-false
   (trie-member
    '(a x)
    (make-trie '((a x y)
                 (c a x y)
                 (a x z)))))
  (untrace make-trie trie= trie-member))

(test ground-problem
  (for-all ((problem
             (with-hash-table-iterator (it *problem-table*)
               (lambda ()
                 (multiple-value-match (it)
                   ((t _ value)
                    value))))))
    (if problem
        (finishes
          (apply #'ground-problem problem))
        (pass "finished"))))
