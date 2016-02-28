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

(defun rel (pathname)
  (asdf:system-relative-pathname :pddl2 pathname))
(defun set-equalp (set1 set2)
  (set-equal set1 set2 :test #'equalp))

(def-suite :pddl2)
(in-suite :pddl2)

;; run test with (run! test-name) 

(test domains
  (for-all ((path (let ((domains (split-sequence
                                  #\Newline
                                  (uiop:run-program (format nil "find ~a -name '*domain*'"
                                                            (rel "t/classical/"))
                                                    :output '(:string :stripped t)))))
                    (lambda ()
                      (random-elt domains)))))
    (finishes
      (read-pddl path))))

(test problems
  #+sbcl
  (setf (sb-ext:bytes-consed-between-gcs)
        (* 1024 1024 100)) ;; MB
  (for-all ((path (let ((domains (split-sequence
                                  #\Newline
                                  (uiop:run-program (format nil "find ~a -name '*.pddl' | grep -v domain"
                                                            (rel "t/classical/"))
                                                    :output '(:string :stripped t)))))
                    (lambda ()
                      (random-elt domains)))))
    (finishes
      (handler-case
          (read-pddl path)
        (unbound-domain ()
          nil)))
    #+sbcl
    (sb-ext:gc :full t)))



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
  (trace make-trie trie-equal trie-member)
  (unwind-protect
      (progn
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
         (trie-equal (make-trie '())
                     (make-trie '())))
        (is-true
         (trie-equal (make-trie '((a x y)))
                     (make-trie '((a x y)))))
        (is-true
         (trie-equal (make-trie '((a x y)
                                  (a x z)))
                     (make-trie '((a x z)
                                  (a x y)))))
        (is-true
         (trie-equal (make-trie '((a x y)
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
        (is (set-equalp
             '((a x y)
               (c a x y)
               (a x z))
             (let (acc)
               (map-trie (lambda (args)
                           (push args acc))
                         (make-trie '((a x y)
                                      (c a x y)
                                      (a x z))))
               acc)))
        (let ((trie (make-trie '((a x y)
                                 (c a x y)
                                 (a x z)))))
          (is (set-equalp
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
                                     (setf trie r2)))))))
          (is (trie-equal trie (make-trie nil))))
        (let ((trie1 (make-trie '((a x y)
                                  (c a x y)
                                  (a x z))))
              (trie2 (make-trie '((a x y)
                                  (c a x y)))))
          (is-false (trie-equal trie1 trie2))
          (push-trie '(a x z) trie2)
          (is (trie-equal trie1 trie2)))
        (let ((trie1 (make-trie '((a x y))))
              (trie2 (make-trie nil)))
          (is-false (trie-equal trie1 trie2))
          (push-trie '(a x y) trie2)
          (is (trie-equal trie1 trie2))))
    (untrace make-trie trie-equal trie-member)))

(test lift-or2
  (is (equal '(and) (lift-or2 `(and)))))


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
      (:goal (at c))))
  (is (trie-equal (make-trie '((at a) (at b) (at c)
                               (connected a b) (connected b c)))
                  (apply #'ground-problem (symbol-problem 'testproblem)))))

(test ground-problem2
  (finishes
    (read-pddl (rel "t/test/domain.pddl"))
    (read-pddl (rel "t/test/probBLOCKS-4-0.pddl"))
    (apply #'ground-problem (symbol-problem 'BLOCKS-4-0))))


(test ground-problem3
  (finishes
    (read-pddl (rel "t/test2/domain.pddl"))
    (read-pddl (rel "t/test2/pfile1.pddl"))
    (apply #'ground-problem (symbol-problem 'DLOG-2-2-2))))

(test ground-problem1-forall
  (finishes
    (define (domain testdomain-adl)
      (:predicates (at ?x) (connected ?x ?y) (true ?x))
      (:action move
               :parameters (?x ?y)
               :precondition (and (at ?x)
                                  (exists (?z) (true ?z))
                                  (not (at ?y))
                                  (connected ?x ?y))
               :effect (and (not (at ?x))
                            (forall (?z) (not (true ?z)))
                            (at ?y)))
      (:action flag
               :parameters ()
               :precondition (exists (?z) (true ?z))
               :effect (forall (?z) (true ?z))))
    (define (problem testproblem-adl)
      (:domain testdomain-adl)
      (:objects a b c)
      (:init (at a) (connected a b) (connected b c) (true c))
      (:goal (at c))))
  (is (trie-equal (make-trie '((at a) (at b) (at c)
                               (connected a b) (connected b c)
                               (true a) (true b) (true c)))
                  (apply #'ground-problem (symbol-problem 'testproblem-adl)))))

(test benchmark1
  (format t "this benchmark could take 5 sec.")
  (finishes
    (read-pddl (rel "t/test3/domain.pddl")))
  (finishes
    (time
     (apply #'ground-problem
            (read-pddl (rel "t/test3/p03.pddl"))))))

(test benchmark2
  (finishes
    (read-pddl (rel "t/classical/nomystery-opt11-strips/domain.pddl"))
    (read-pddl (rel "t/classical/nomystery-opt11-strips/p17.pddl")))
  ;; cd /home/guicho/repos/lisp/pddl2/t/classical/nomystery-opt11-strips
  ;; time ~/repos/lisp/mwup/downward/translate/translate.py domain.pddl p17.pddl
  ;; Runtime by FD's translate.py:
  ;; real	0m2.686s
  ;; user	0m2.372s
  ;; sys	0m0.084s
  (format t "this benchmark could take about 6 sec.")
  ;; initially
  ;; Evaluation took:
  ;;   123.177 seconds of real time
  ;;   121.148000 seconds of total run time (120.976000 user, 0.172000 system)
  ;;   [ Run times consist of 0.684 seconds GC time, and 120.464 seconds non-GC time. ]
  ;;   98.35% CPU
  ;;   369,551,213,802 processor cycles
  ;;   9,879,499,280 bytes consed
  ;; finally
  ;; Evaluation took:
  ;;   5.656 seconds of real time
  ;;   5.344000 seconds of total run time (5.328000 user, 0.016000 system)
  ;;   [ Run times consist of 0.072 seconds GC time, and 5.272 seconds non-GC time. ]
  ;;   94.48% CPU
  ;;   16,970,251,797 processor cycles
  ;;   1,077,361,888 bytes consed
  (finishes
   (time
    (apply #'ground-problem (symbol-problem 'transport-l10-t1-p9---int100n150-m25---int100c110---s1---e0)))))

(test benchmark3
  (finishes
    (read-pddl (rel "t/classical/visitall-sat14-strips/domain.pddl"))
    (read-pddl (rel "t/classical/visitall-sat14-strips/pfile59.pddl")))
  ;; cd /home/guicho/repos/lisp/pddl2/t/classical/visitall-sat14-strips
  ;; time ~/repos/lisp/mwup/downward/translate/translate.py domain.pddl pfile59.pddl
  ;; real	0m3.738s
  ;; user	0m3.436s
  ;; sys	0m0.068s
  (format t "this benchmark could take about 6 sec.")
  ;; initially
  ;; Evaluation took:
  ;;   89.741 seconds of real time
  ;;   ...
  ;;   before it was aborted by a non-local transfer of control.
  ;; finally
  ;; Evaluation took:
  ;;   5.656 seconds of real time
  ;;   5.344000 seconds of total run time (5.328000 user, 0.016000 system)
  ;;   [ Run times consist of 0.072 seconds GC time, and 5.272 seconds non-GC time. ]
  ;;   94.48% CPU
  ;;   16,970,251,797 processor cycles
  ;;   1,077,361,888 bytes consed
  (finishes
   (time
    (apply #'ground-problem (symbol-problem 'grid-59)))))



;; #+nil
(test ground-problem4
  (time
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
            (pass "finished")))))))
