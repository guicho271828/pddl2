;;; package
(in-package :pddl2.impl)


(defvar *ground-predicates*)
(defvar *ground-actions*)
(defvar *ground-axioms*)

(defun ground-problem (*requirements*
                       *types*
                       *objects*
                       *predicates*
                       *numeric-fluents*
                       *object-fluents*
                       *axioms*
                       *actions*
                       ;;
                       *domain*
                       *domain-name*
                       *init*
                       *metric*
                       *ground-numeric-fluents*
                       *ground-object-fluents*)
  ;; grounding target: predicates, actions, axioms
  ;; does some reachability analysis based on relaxed planning graph
  (make-trie *init*)
  (make-trie *ground-numeric-fluents*)
  (make-trie *ground-object-fluents*)
  (make-trie *actions*)
  (make-trie *axioms*))

(defun map-plist (fn plist)
  (iter (for (key value . rest) on plist by #'cddr)
        (collect (funcall fn key value))))

(defun make-trie (list)
  (let (acc (list (remove-duplicates list :test #'equal)))
    (iter (for (head . rest) in list)
          (push rest (getf acc head)))
    ;; (sleep 1)
    (map-plist (lambda (head children)
                 (if (equalp children '(()))
                     (list head)
                     (cons head (make-trie children))))
               acc)))

(print
 (make-trie '((a x y)
              (b x y z)
              (a x z)
              (b x z y))))

;; ((B (X (Z (Y)) (Y (Z)))) (A (X (Z) (Y))))

(print (make-trie '((a) (b))))
(print (make-trie '((a) (a))))

;; (action-layer init)

(defun action-layer (propositions)
  )

(defun fact-layer (propositions actions)
  )

