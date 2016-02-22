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
  
  (print
   (reachable-predicates
    (print
     (make-trie *init*))))
  ;; (print
  ;;  (make-trie *ground-numeric-fluents*))
  ;; (print
  ;;  (make-trie *ground-object-fluents*))
  ;; (print
  ;;  (make-trie *actions*))
  ;; (make-trie *axioms*)
  )

;; axioms are later
;; (mapcar (compose #'enqueue #')
;;         (remove-if-not (curry #'derivable reachable)
;;                        *axioms*))


(defun fact-based-exploration ()
  "cf. Exhibiting Knowledge in Planning Problems to Minimize State Encoding Length, Edelkamp, Helmert"
  (let* ((queue (copy-list *init*))
         (last (last queue))
         (reachable (make-trie nil))
         (ground-actions (make-trie nil)))
    (flet ((enqueue (thing)
             (let ((new (cons thing nil)))
               (setf (cdr last) new
                     last new))))
      (iter (while queue)
            (setf reachable
                  (merge-trie reachable
                              (make-trie (list (pop queue)))))
            (for gas = (mappend (lambda (a)
                                  (new-applicable-action-skeletons a reachable ground-actions))
                                *actions*))
            (map nil (compose #'enqueue #'add-effects) gas)
            (setf ground-actions
                  (merge-trie ground-actions
                              (make-trie gas)))))))

(defun new-applicable-action-skeletons (action reachable ground-actions)
  "action definition, trie of facts, trie of action skeletons"
  (ematch action
    ((list name
           :parameters params
           :precondition (list* 'or rest)
           :effect eff)
     (mappend (lambda (precond)
                (new-applicable-action-skeletons (list name
                                                       :parameters params
                                                       :precondition precond
                                                       :effect eff)
                                                 reachable ground-actions))
              rest))
    (_
     (%applicable-bindings action reachable ground-actions))))



(defvar *bindings*)
(defun %applicable-bindings (action reachable ground-actions)
  ;; list, list, trie, trie, trie
  ;; (unload-truck ?pkg ?place1 ?place2)
  ;; (at ?package ?place1)
  ;; (package ?package)
  ;; (place ?place1)
  (match* (action reachable)
    (((list* name
            :parameters nil
            :precondition _)
      _)
     (list *bindings*))
    (((list name
            :parameters params
            :precondition (list* 'and (list* p-head p-params) _)
            :effect eff)
      ;; trivia's assoc pattern
      (assoc p-head trie))
     ;; start backtrack
     (let (acc)
       (map-trie (lambda (args)
                   ;; new binding
                   (let* ((new-bindings (remove-if-not #'variablep
                                                       (mapcar #'cons p-params args)
                                                       :key #'car))
                          (newaction (reduce #'bind-action new-bindings :initial-value action))
                          (*bindings* (append *bindings* new-bindings)))
                     (appendf acc (%applicable-bindings newaction reachable ground-actions))))
                 trie)
       acc))))


;;; enumerate reachable predicates

(defcached bind-action (action binding)
  (ematch* (action binding)
    (((list name
           :parameters params
           :precondition precond
           :effect eff)
      (cons parameter object))
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

