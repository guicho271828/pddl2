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

;; (defun make-trie (list)
;;   `(+root+ ,@(%make-trie list)))

(defun merge-trie (t1 t2)
  (let ((acc (copy-tree t1)))
    (iter (for subtrie2 in t2)
          (for (head . rest) = subtrie2)
          (if-let ((subtrie1 (assoc head acc)))
            (setf (cdr subtrie1)
                  (merge-trie (cdr subtrie1) rest))
            (push subtrie2 acc)))
    acc))

(defun trie= (t1 t2)
  (iter (for subtrie2 in t2)
        (for (head . rest) = subtrie2)
        (always
         (when-let ((subtrie1 (assoc head t1)))
           (trie= (cdr subtrie1) rest)))))

(defun trie-member (list trie)
  (match list
    ((cons head nil)
     (match (assoc head trie)
       ((cons _ nil)
        t)))
    ((cons head rest)
     (match (assoc head trie)
       ((cons _ rest2)
        (trie-member rest rest2))))))

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

