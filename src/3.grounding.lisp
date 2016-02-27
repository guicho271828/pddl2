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
                       *domain-name*
                       *init*
                       *metric*
                       *ground-numeric-fluents*
                       *ground-object-fluents*
                       *current-pathname*)
  ;; grounding target: predicates, actions, axioms
  ;; does some reachability analysis based on relaxed planning graph
  (format t "~%grounding a problem ~a" *current-pathname*)
  (print (fact-based-exploration *init*))
  ;; (print
  ;;  (reachable-predicates
  ;;   (print
  ;;    (make-trie *init*))))
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


(defun fact-based-exploration (init)
  "cf. Exhibiting Knowledge in Planning Problems to Minimize State Encoding Length, Edelkamp, Helmert"
  (let* ((queue (make-trie init))
         (reachable (make-trie nil))
         (instantiated-actions (make-trie nil))
         (e 0) (loge 0)
         (d 0) (logd 0))
    (flet ((enqueue (thing)
             ;; #+nil
             ;; (format t "~&; E ~a" thing)
             ;; (fresh-line)
             (push-trie thing queue)
             ;; (format t "~&; Q ~a" queue)
             ;; (format t "~&; R ~a" reachable)
             (incf e)
             (let ((loge-new (floor (log e 2))))
               (when (< loge loge-new)
                 (format t "~&; ~ath enqueue" e)
                 (setf loge loge-new))))
           (dequeue ()
             (multiple-value-bind (r1 r2) (pop-trie queue)
               (prog1 r1 (setf queue r2)
                      ;; #+nil
                      ;; (format t "~&; D ~a" r1)
                      ;; (format t "~&; Q ~a" queue)
                      ;; (format t "~&; R ~a" reachable)
                      ;; (fresh-line)
                      (incf d)
                      (let ((logd-new (floor (log d 2))))
                        (when (< logd logd-new)
                          (format t "~&; ~ath deque" d)
                          (setf logd logd-new)))))))
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

(defun ground-actions (action reachable)
  "action definition, trie of facts, trie of action skeletons"
  (ematch action
    ((list name
           :parameters params
           :precondition (list* 'or rest)
           :effect eff)
     (mappend (lambda (precond)
                (ground-actions (list name
                                      :parameters params
                                      :precondition precond
                                      :effect eff)
                                reachable))
              rest))
    (_
     (%applicable-bindings action reachable nil))))

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

(defun check-action (action reachable)
  "Returns T if the action is applicable to the reachable sets of states, if lifted variables are ignored"
  (labels ((check-condition (condition)
             (ematch condition
               ((list* 'and conditions)
                (every #'check-condition conditions))
               ((list* 'or conditions)
                (some #'check-condition conditions))
               ((list 'not (list* head params))
                (~test-parameter head params (cdr (assoc head reachable))))
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

(defun ~test-parameter (head params obj-trie)
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


;;; extract the effect

(defun add-effects (gaction)
  (ematch gaction
    ((list* name objs)
     (ematch (reduce #'bind-action1 objs :initial-value (assoc name *actions*))
       ((list _
              :parameters nil
              :precondition _
              :effect (list* 'and effects))
        (remove-if (lambda-match
                     ((list* (or 'not 'assign 'increase 'decrease 'scale-up 'scale-down) _)
                      t))
                   effects))))))




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
