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
  )

;;; enumerate reachable predicates

(defun bind-action (action parameter object)
  (ematch action
    ((list name
           :parameters params
           :precondition precond
           :effect eff)
     (assert (member parameter params))
     (list name
           :parameters (remove parameter params)
           :precondition (subst object parameter precond)
           :effect (subst object parameter eff)))))





;; (defun reachable-predicates (trie)
  


;; (action-layer init)

(defun action-layer (propositions)
  )

(defun fact-layer (propositions actions)
  )

