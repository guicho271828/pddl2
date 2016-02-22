
;;; package
(in-package :pddl2.impl)

(defvar *domain*)
(defvar *domain-name*)
(defvar *ground-predicates*)
(defvar *ground-actions*)
(defvar *ground-axioms*)

(defvar *ground-numeric-fluents*)
(defvar *ground-object-fluents*)

(defvar *init*)
(defvar *goal*)
(defvar *metric*)

(defun parse-problem (body)
  (let (*requirements*
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
        *goal*
        *ground-predicates*
        *ground-actions*
        *ground-axioms*
        *ground-numeric-fluents*
        *ground-object-fluents*
        *metric*)
    (iter (for (c-kind . c-body) in body)
          (funcall #'process-clause c-kind c-body))
    ;; removing disjunctive conditions
    (setf *actions* (really-process-actions *actions*)
          *axioms*  (really-process-axioms *axioms*))
    ;; grounding actions, axioms and functions
    (let ((*print-length* 8))
      (print (list
              *init*
              *goal*
              *ground-predicates*
              *ground-actions*
              *ground-axioms*
              *ground-numeric-fluents*
              *ground-object-fluents*
              *metric*)))))
;;; process clauses
(defmethod process-clause ((clause (eql :domain)) body)
  (ematch body
    ((list domain-name)
     (setf *domain* (symbol-domain domain-name)
           *domain-name* domain-name
           (values *requirements*
                   *types*
                   *objects*
                   *predicates*
                   *numeric-fluents*
                   *object-fluents*
                   *axioms*
                   *actions*)
           (values-list *domain*)))))

(defmethod process-clause ((clause (eql :objects)) body)
  ;; apppend to the constants
  (appendf *objects* (parse-typed-list body)))

(defmethod process-clause ((clause (eql :init)) body)
  (setf (values *numeric-fluents* *object-fluents* *init*)
        (iter (for elem in body)
              (ematch elem
                ((list '= place (and value (number)))
                 (collect (cdr elem) into nf))
                ((list '= place (and value (symbol)))
                 (collect (cdr elem) into of))
                (_
                 (collect elem into init)))
              (finally
               (return (values nf of
                               (append init (types-as-predicates *objects*))))))))

(defun types-as-predicates (params)
  (iter outer
        (for (p . type) in params)
        (for types = (assoc type *types*))
        (iter (for type in types)
              (in outer
                  (collect `(,type ,p))))))

(defmethod process-clause ((clause (eql :goal)) body)
  (push *actions*
        `(:action *goal*
          :parameters ()
          :precondition ,body
          :effect (and))))

(defmethod process-clause ((clause (eql :metric)) body)
  (setf *metric* body))

;;; process actions

(defvar *disjunctions*)

(defun lift-adl (body)
  ;; returns a disjunction of conjunctions
  (destructuring-bind (disjunctions con) (%lift-adl body)
    (if disjunctions
        (apply #'map-product
               (lambda (&rest args)
                 (reduce #'append args :initial-value con :from-end t))
               (iter (for disjunction in disjunctions)
                     (collect
                      (iter (for disjunction-term in disjunction)
                            (appending (lift-adl disjunction-term))))))
        (list con))))

(defun %lift-adl (body)
  (let (*disjunctions*)
    (let ((conjunction (%%lift-adl body)))
      (list *disjunctions* conjunction))))

(defun %%lift-adl (body)
  (match body
    ((list* 'and rest)
     (mappend #'%%lift-adl rest))
    ((list* 'or rest)
     (push rest *disjunctions*)
     nil)
    ((list* 'forall params quantified-body)
     (let* ((params (parse-typed-list params t))
            (matched (iter (for (p . type) in params)
                           (collect
                               (iter (for (o . ot) in *objects*)
                                     (for supers = (assoc ot *types*))
                                     (when (member type supers)
                                       (collect o)))))))
       (map-product (lambda (&rest args)
                      (iter (with body = quantified-body)
                            (for p in (mapcar #'first params))
                            (for o in args)
                            (setf body (subst o p body))
                            (finally (return body))))
                    matched)))
    ((list* 'exists params quantified-body)
     (let* ((params (parse-typed-list params t))
            (matched (iter (for (p . type) in params)
                           (collect
                               (iter (for (o . ot) in *objects*)
                                     (for supers = (assoc ot *types*))
                                     (when (member type supers)
                                       (collect o))))))
            (grounded (map-product (lambda (&rest args)
                                     (iter (with body = quantified-body)
                                           (for p in (mapcar #'first params))
                                           (for o in args)
                                           (setf body (subst o p body))
                                           (finally (return body))))
                                   matched)))
       (push grounded *disjunctions*)
       nil))
    ((list* 'imply lhs rhs)
     (push (list `(not ,lhs) rhs) *disjunctions*)
     nil)
    ((list 'not (list* 'and rest))
     (%%lift-adl `(or ,@(mapcar (lambda (x) `(not ,x)) rest))))
    ((list 'not (list* 'or rest))
     (mappend #'%%lift-adl rest))
    ((list 'not (list* 'forall params quantified-body))
     (%%lift-adl `(exists ,params (not ,quantified-body))))
    ((list 'not (list* 'exists params quantified-body))
     (%%lift-adl `(forall ,params (not ,quantified-body))))
    ((list 'not (list* 'imply lhs rhs))
     ;; t t -> t -> n
     ;; t n -> n -> t
     ;; n t -> t -> n
     ;; n n -> t -> n
     (%%lift-adl `(and ,lhs (not ,rhs))))
    (_
     (list body))))





(defun really-process-actions (proto-actions)
  (let ((proto-actions (copy-list proto-actions))
        (last (last proto-actions)))
    (iter (for proto-action on proto-actions)
          (restart-bind
              ((push-action
                (lambda (action)
                  (setf (cdr last) (cons action nil)
                        last (cdr last))))
               (skip-action
                (lambda ()
                  (next-iteration))))
            (appending
             (really-process-action proto-action))))))

(defun really-process-action (proto-action)
  (ematch proto-action
    ((list* name
            :parameters params
            :precondition precond
            :effect eff)
     (let* ((params (parse-typed-list params t))
            (param-names (mapcar #'car params))
            (type-predicates (types-as-predicates params))
            (precond-disjunctions
             ;; list of disjunctive formulas
             (remove-duplicates
              (lift-adl `(and ,@type-predicates ,precond))
              :test #'equalp)))
       (multiple-value-bind (static-effects conditional-effects) (parse-effect eff)
         (map-product
          (lambda-match*
            ((conjunction (cons condition effect))
             ;; merge the preconditions and the conditions of conditional effects
             (list param-names
                   (append conjunction effect-conjunction)
                   effect)))
          precond-disjunctions conditional-effects))))))

(defun parse-effect (body)
  (match body
    ((list* 'and rest)
     (mappend #'%%parse-effect rest))
    ((list* 'forall params quantified-body)
     (let* ((params (parse-typed-list params t))
            (matched (iter (for (p . type) in params)
                           (collect
                               (iter (for (o . ot) in *objects*)
                                     (for supers = (assoc ot *types*))
                                     (when (member type supers)
                                       (collect o)))))))
       (map-product (lambda (&rest args)
                      (iter (with body = quantified-body)
                            (for p in (mapcar #'first params))
                            (for o in args)
                            (setf body (subst o p body))
                            (finally (return body))))
                    matched)))
    ((list* 'when condition effect)
     
     )
    ((list* 'increase (list 'total-cost) quantity)
     (list `(:cost ,body)))
    ((list* 'increase _ _)
     (error "Increasing places other than (TOTAL-COST) is not supported : ~a" body))
    ((list* (and invalid (or 'decrease 'assign 'scale-up 'scale-down)) _ _)
     (error "general numeric fluent ~a is not supported : ~a" invalid body))
    ((list 'not (list* (and invalid (or 'and 'or 'forall 'exists 'imply)) _))
     (error "~a in atomic formula : ~a" invalid body))
    ((list 'not atomic-formula)
     (list `(:del ,body)))
    (_
     (list `(:add ,body)))))
