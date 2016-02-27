;;; package
(in-package :pddl2.impl)

;; fact-based-exploration1
;; 8.313 |      0.200 | 2,494,250,208 | 4,811,016 |   0.000002 | PDDL2.IMPL::BIND-ACTION1
;; fact-based-exploration2
;; 6.675 |      0.152 | 2,017,032,720 | 3,888,427 |   0.000002 | PDDL2.IMPL::BIND-ACTION1

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
  (format t "~%grounding a problem ~a" *current-pathname*)
  (print (fact-based-exploration2 *init*)))

;; axioms are later
;; (mapcar (compose #'enqueue #')
;;         (remove-if-not (curry #'derivable reachable)
;;                        *axioms*))

(defun log-logger (what)
  (let ((c 0) (log 0))
    (lambda ()
      (incf c)
      (let ((log-new (floor (log c 2))))
        (when (< log log-new)
          (format t "~&; ~ath ~a" c what)
          (setf log log-new))))))

(defun bind-action1 (action object)
  "bind the first parameter"
  (ematch action
    ((list name
           :parameters (list* parameter rest)
           :precondition precond
           :effect eff)
     (list name
           :parameters rest
           :precondition (subst object parameter precond)
           :effect (subst object parameter eff)))))

(defun nbind-action1 (action object)
  "bind the first parameter: destructive"
  (ematch action
    ((list name
           :parameters (list* parameter rest)
           :precondition (place precond)
           :effect (place eff))
     (setf precond (nsubst object parameter precond)
           eff     (nsubst object parameter eff))
     action)))


(defun check-action (action reachable)
  "Returns T if the action is applicable to the reachable sets of states, if lifted variables are ignored"
  (labels ((check-condition (condition)
             (ematch condition
               ((list* 'and conditions)
                (every #'check-condition conditions))
               ((list* 'or conditions)
                (some #'check-condition conditions))
               ((list 'not _)
                t)
               ((list* head params)
                (test-parameter head params (cdr (assoc head reachable)))))))
    (ematch action
      ((list* _
              :parameters _
              :precondition precond _)
       (check-condition precond)))))

(defun test-parameter (head params obj-trie)
  "test if the given list of parameters is have a possible binding"
  (ematch params
    (nil t)
    ((list* p ps)
     (if (variablep p)
         (iter (for (obj . subtrie) in obj-trie)
               (thereis
                (test-parameter head ps subtrie)))
         (when-let ((found (assoc p obj-trie)))
           (test-parameter head ps (cdr found)))))))

;;; extract the effect

(defun add-effects (gaction)
  (ematch gaction
    ((list* name objs)
     (ematch (reduce #'bind-action1 objs :initial-value (assoc name *actions*))
       ((list _
              :parameters nil
              :precondition _
              :effect effects)
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
                       ((list* (or 'not 'assign 'increase 'decrease 'scale-up 'scale-down) _)
                        ;; ignore negative effect and costs
                        nil)
                       (_ (push condition acc)))))
            (walk-effect effects)
            acc)))))))

;;; old

(defun bind-action-old (action binding)
  ;; binding : (param . obj)
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

(defun ~test-parameter (head params obj-trie)
  ;; no more used, since checking the negative predicate is not necessary
  "test if the given list of parameters have a possible binding --- for negative predicates"
  (ematch params
    (nil nil)
    ((list* p ps)
     (if (variablep p)
         (or (iter (for (obj . type) in *objects*)
                   (thereis
                    (not (assoc obj obj-trie))))
             (iter (for (obj . subtrie) in obj-trie)
                   (thereis
                    (~test-parameter head ps subtrie))))
         (if-let ((found (assoc p obj-trie)))
           (~test-parameter head ps (cdr found))
           t)))))


(defun fact-based-exploration1 (init)
  "cf. Exhibiting Knowledge in Planning Problems to Minimize State Encoding Length, Edelkamp, Helmert"
  (let* ((queue (make-trie init))
         (reachable (make-trie nil))
         (instantiated-actions (make-trie nil)))
    (flet ((enqueue (thing)
             (push-trie thing queue)
             (log-logger :enqueue))
           (dequeue ()
             (multiple-value-bind (r1 r2) (pop-trie queue)
               (prog1 r1 (setf queue r2)
                      (log-logger :dequeue)))))
      (iter (while queue)
            (push-trie (dequeue) reachable)
            (for gas = (mappend (lambda (a)
                                  (ground-actions a reachable))
                                *actions*))
            (dolist (ga gas)
              (push-trie ga instantiated-actions)
              (dolist (ae (add-effects ga))
                (unless (trie-member ae reachable)
                  (enqueue ae)))))
      (values reachable instantiated-actions))))

(defun fact-based-exploration2 (init)
  "cf. Exhibiting Knowledge in Planning Problems to Minimize State Encoding Length, Edelkamp, Helmert"
  (let* ((queue (make-trie init))
         (reachable (make-trie nil))
         (instantiated-actions (make-trie nil))
         (alist (make-predicate-action-map)))
    (print alist)
    (flet ((enqueue (thing)
             (push-trie thing queue)
             (log-logger :enqueue))
           (dequeue ()
             (multiple-value-bind (r1 r2) (pop-trie queue)
               (prog1 r1 (setf queue r2)
                      (log-logger :dequeue)))))
      (iter (while queue)
            (for new = (dequeue))
            (push-trie new reachable)
            (for gas = (mappend (lambda (a)
                                  (ground-actions a reachable))
                                (ematch new
                                  ((list* head _)
                                   (cdr (assoc head alist))))))
            (dolist (ga gas)
              (push-trie ga instantiated-actions)
              (dolist (ae (add-effects ga))
                (unless (trie-member ae reachable)
                  (enqueue ae)))))
      (values reachable instantiated-actions))))

(defun make-predicate-action-map ()
  (let (alist)
    (iter (for a in *actions*)
          (ematch a
            ((list* '*goal* _) nil)
            ((list* _
                    :parameters _
                    :precondition precond _)
             (labels ((walk-condition (condition)
                        (ematch condition
                          ((list* 'and conditions)
                           (map nil #'walk-condition conditions))
                          ((list* 'or conditions)
                           (map nil #'walk-condition conditions))
                          ((list 'not _)
                           ;; ignored
                           nil)
                          ((list* head _)
                           (let ((pair (assoc head alist)))
                             (if pair
                                 (pushnew a (cdr pair))
                                 (push (cons head (list a)) alist)))))))
               (walk-condition precond)))))
    alist))

(defun ground-actions (action reachable)
  "action definition, trie of facts, trie of action skeletons"
  (%applicable-bindings action reachable nil))

(defun %applicable-bindings (action reachable bindings)
  ;; list, list, trie, trie, trie
  (match action
    ((list* name :parameters nil _)
     (list (cons name (reverse bindings))))
    (_
     (iter (for (o . type) in *objects*)
           ;; ignore type
           (let ((partial (bind-action1 action o)))
             (when (check-action partial reachable)
               (appending (%applicable-bindings partial reachable (cons o bindings)))))))))
