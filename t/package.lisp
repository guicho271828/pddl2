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
  (is (set-equal
       '((a x y)
         (c a x y)
         (a x z))
       (let (acc)
         (map-trie (lambda (args)
                     (push args acc))
                   (make-trie '((a x y)
                                (c a x y)
                                (a x z))))
         acc)
       :test #'equalp))
  (let ((trie (make-trie '((a x y)
                           (c a x y)
                           (a x z)))))
    (is (set-equal
         '((a x y)
           (c a x y)
           (a x z))
         (print (list (multiple-value-bind (r1 r2) (pop-trie trie)
                        (prog1 r1
                               (setf trie r2)))
                      (multiple-value-bind (r1 r2) (pop-trie trie)
                        (prog1 r1
                               (setf trie r2)))
                      (multiple-value-bind (r1 r2) (pop-trie trie)
                        (prog1 r1
                               (setf trie r2)))))
         :test #'equalp))
    (is (trie= trie (make-trie nil))))
  (untrace make-trie trie= trie-member))

(test ground-problem1
  (finishes
    (define (domain testdomain)
      (:predicates (at ?x) (connected ?x ?y))
      (:action move
               :parameters (?x ?y)
               :precondition (and (at ?x)
                                  (connected ?x ?y))
               :effect (and (not (at ?x))
                            (at ?y))))
    (define (problem testproblem)
      (:domain testdomain)
      (:objects a b c)
      (:init (at a) (connected a b) (connected b c))
      (:goal (at c)))
    (apply #'ground-problem (symbol-problem 'testproblem))))

(test ground-problem2
  (finishes
    (read-pddl (merge-pathnames "t/test/domain.pddl" *default-pathname-defaults*))
    (read-pddl (merge-pathnames "t/test/probBLOCKS-4-0.pddl" *default-pathname-defaults*))
    (apply #'ground-problem (symbol-problem 'BLOCKS-4-0))))


(test ground-problem3
  (finishes
    (read-pddl (merge-pathnames "t/test2/domain.pddl" *default-pathname-defaults*))
    (read-pddl (merge-pathnames "t/test2/pfile1.pddl" *default-pathname-defaults*))
    (apply #'ground-problem (symbol-problem 'DLOG-2-2-2))))

(test ground-problem1-neg
  (finishes
    (define (domain ~testdomain)
      (:predicates (at ?x) (connected ?x ?y))
      (:action move
               :parameters (?x ?y)
               :precondition (and (at ?x)
                                  (not (at ?y))
                                  (connected ?x ?y))
               :effect (and (not (at ?x))
                            (at ?y))))
    (define (problem ~testproblem)
      (:domain ~testdomain)
      (:objects a b c)
      (:init (at a) (connected a b) (connected b c))
      (:goal (at c)))
    (apply #'ground-problem (symbol-problem '~testproblem))))


(test ground-problem4
  (for-all ((problem
             (with-hash-table-iterator (it *problem-table*)
               (lambda ()
                 (multiple-value-match (it)
                   ((t key value)
                    (list key value)))))))
    (match problem
      ((list key problem) 
       (if problem
           (finishes
             (format t "~&instantiating ~A~&" key)
             (time (apply #'ground-problem problem)))
           (pass "finished"))))))
