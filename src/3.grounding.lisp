;;; package
(in-package :pddl2.impl)

#|

fact-based-exploration1
bind-action1 :
  9.279 seconds of real time
  9.308000 seconds of total run time (9.256000 user, 0.052000 system)
  27,837,749,823 processor cycles
  2,510,101,264 bytes consed

nbind-action1 :
  8.333 seconds of real time
  8.364000 seconds of total run time (8.364000 user, 0.000000 system)
  24,999,240,983 processor cycles
  2,265,628,416 bytes consed

fact-based-exploration2
bind-action1 :
  7.377 seconds of real time
  7.400000 seconds of total run time (7.368000 user, 0.032000 system)
  22,131,262,925 processor cycles
  2,030,614,080 bytes consed

nbind-action1 :
  7.142 seconds of real time
  7.164000 seconds of total run time (7.160000 user, 0.004000 system)
  21,425,355,510 processor cycles
  1,971,950,016 bytes consed

fact-based-exploration3

  0.265 seconds of real time
  0.272000 seconds of total run time (0.272000 user, 0.000000 system)
  792,419,679 processor cycles
  70,281,488 bytes consed

|#


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
                       *domain-name*
                       *init*
                       *metric*
                       *ground-numeric-fluents*
                       *ground-object-fluents*
                       *current-pathname*)
  ;; grounding target: predicates, actions, axioms
  ;; does some reachability analysis based on relaxed planning graph
  (format t "~%grounding a problem ~a~%" *current-pathname*)
  (print (fact-based-exploration *init*)))

;; axioms are later
;; (mapcar (compose #'enqueue #')
;;         (remove-if-not (curry #'derivable reachable)
;;                        *axioms*))

(defun fact-based-exploration (init)
  "cf. Exhibiting Knowledge in Planning Problems to Minimize State Encoding Length, Edelkamp, Helmert"
  (let* ((fact-queue (make-trie init))
         (reachable (make-trie nil))
         (instantiated-actions (make-trie nil))
         (l (log-logger)))
    (macrolet ((enqueue (thing queue)
                 `(progn (push-trie ,thing ,queue)
                         (funcall l :enqueue)))
               (dequeue (queue)
                 `(multiple-value-bind (r1 r2) (pop-trie ,queue)
                    (prog1 r1 (setf ,queue r2) (funcall l :dequeue)))))
      (iter (while fact-queue)
            (for new = (dequeue fact-queue))
            (push-trie new reachable)
            (dolist (ga (ground-actions new reachable))
              (push-trie ga instantiated-actions)
              (dolist (ae (add-effects (grounded-action-definition ga)))
                (unless (trie-member ae reachable)
                  (enqueue ae fact-queue)))))
      (values reachable instantiated-actions))))

(defun ground-actions (new-fact reachable)
  "Compute the set of actions enabled by new-fact"
  (flet ((parameters (action)
           (ematch action ((list* _ :parameters params _) params))))
   (ematch new-fact
     ((list* head args)
      (iter outer
            (for (action . p-params) in (action-requiring head))
            (for bindings = nil)
            (iter (for p in params)
                  (for o in args)
                  (if (variablep p) ; params may contain constants.
                      (if-let ((binding (assoc p bindings)))
                        ;; a parameter can appear twice e.g.: (pred ?X ?Y ?X)
                        (unless (eq o (cdr binding)) ; in that case, it should be eq to the established binding.
                          (in outer (next-iteration)))
                        (push (cons p o) bindings))
                      (unless (eq p o) ; when it is a constant, it should be eq to the argument.
                        (in outer (next-iteration)))))
            (for partial-action = (reduce #'nbind-action bindings :initial-value (copy-tree action)))
            ;; some arguments are partially grounded.
            ;; For example, ?X of (move ?X ?Y) may be curried here.
            (for ground-action-skeletons = (ground-actions partial-action reachable))
            ;; (print ground-action-skeletons)
            ;; ((move A) (move B)) --- since the parameters given to
            ;; ground-actions are partial, the results are also partial.
            ;; thus, we have to restore the original arguments.
            (iter (for (action-name . args) in ground-action-skeletons)
                  (for remaining-binding = ; ((?Y . A))
                       (mapcar #'cons (parameters partial-action) args))
                  (for whole-binding = ; ((?X . C) (?Y . A))
                       (append bindings remaining-binding))
                  (in outer
                      (collect
                          (cons action-name
                                (iter (for p in (parameters action))
                                      (collect (cdr (assoc p whole-binding)))))))))))))

(defun action-requiring (predicate-head)
  (getf (p-a-mapping *actions*) predicate-head))

(defcached p-a-mapping (actions)
  "Walk over the preconditions, collecting the positive predicates (requiring some predicates to be true).
Those requirements are tied to the action name, enabling a lookup from
predicate -> action. This is useful when a new fact is introduced, and you
have to find the actions that are affected"
  (let (plist)
    (iter (for a in actions)
          (ematch a
            ((list* '*goal* _)) ; ignore
            ((list* (not '*goal*) :parameters _ :precondition precond _)
             (labels ((walk-condition (condition)
                        (ematch condition
                          ((list* (op _) conditions)
                           (map nil #'walk-condition conditions))
                          ((list 'not _)) ; ignored
                          ((list* head params)
                           (push (cons a params) (getf plist head))))))
               (walk-condition precond)))))
    plist))

;;; extract the effect

(defun grounded-action-definition (gaction)
  (ematch gaction
    ((list* name objs)
     (ematch (assoc name *actions*)
       ((and action
             (list* (eq name)
                    :parameters params _))
        (assert (= (length params) (length objs)))
        (reduce #'nbind-action1 objs :initial-value (copy-tree action)))))))

(defun add-effects (action)
  (ematch action
    ((list _ :parameters _ :precondition _ :effect effects)
     (let (acc)
       (labels ((walk-effect (condition)
                  (ematch condition
                    ((list* 'and conditions)
                     (every #'walk-effect conditions))
                    ((or (list 'not (list* (or 'and 'or 'forall 'exists 'imply) _))
                         (list* (or 'or 'exists 'imply) _))
                     (error "invalid effect: ~a" condition))
                    ((list* (and op (or 'forall 'when)) _)
                     (error "~A should have been compiled away?: ~a" op condition))
                    ;; ignore negative effect and costs
                    ((list* (or 'not 'assign 'increase 'decrease 'scale-up 'scale-down) _))
                    (_ (push condition acc)))))
         (walk-effect effects)
         acc)))))

