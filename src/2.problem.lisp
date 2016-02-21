
;;; package
(in-package :pddl2.impl)

(defvar *domain*)
(defvar *domain-name*)
(defvar *ground-predicates*)
(defvar *ground-actions*)
(defvar *ground-axioms*)
(defvar *ground-functions*)
(defvar *init*)
(defvar *goal*)
(defvar *metric*)

(defun parse-problem (body)
  (let (*requirements*
        *types*
        *objects*
        *predicates*
        *functions*
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
        *ground-functions*
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
              *ground-functions*
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
                   *functions*
                   *axioms*
                   *actions*)
           (values-list *domain*)))))

(defmethod process-clause ((clause (eql :objects)) body)
  (appendf *objects* (parse-typed-list body)))

(defmethod process-clause ((clause (eql :init)) body)
  (setf (values *init* *ground-functions*)
        (let ((body (append (types-as-predicates *objects*) body)))
          (values (remove '= body :key #'first)
                  (mapcar #'cdr
                          (remove '= body :key #'first :test (complement #'eq))))))
  (iter (for gf in *ground-functions*)
        (assert (match gf
                  ((list _ (number)) t))
                nil
                "object fluents in ~a !" gf)))

(defun types-as-predicates (params)
  (iter outer
        (for (p . type) in params)
        (for types = (assoc type *types*))
        (iter (for type in types)
              (in outer
                  (collect `(,type ,p))))))

(defvar *disjunctions*)

(defmethod process-clause ((clause (eql :goal)) body)
  (setf *goal* (lift-adl (first body))))

;; (or (and (or 1 2)
;;          4)
;;     3)
;; 
;; (or (and 1 4)
;;     (and 2 4)
;;     3)
;; 
;; 
;; = (or 1 2 3)

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




(defmethod process-clause ((clause (eql :metric)) body)
  (setf *metric* body)
  (assert (match body
            ((list 'minimize (list 'total-cost)) t))
          nil
          "We do not support costs other than 'minimize' and 'total-cost'! : ~a" body))

;;; process actions

(defun really-process-actions (proto-actions)
  (let ((proto-actions (copy-list proto-actions)))
    (iter (for proto-action on proto-actions)
          (restart-bind
              ((push-action
                (lambda (action)
                  (setf (cdr (last proto-actions)) (cons action nil))))
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
              :test #'equalp))
            (effects
             ;; list of: < list of conjunctions . list of effects >
             ;; conditions originate from the `when' clause (conditional effects)
             (parse-effect eff)))
       (map-product
        (lambda-match*
          ((conjunction (cons effect-conjunction effect))
           ;; merge the preconditions and the conditions of conditional effects
           (list param-names
                 (append conjunction effect-conjunction)
                 effect)))
        precond-disjunctions effects)))))

(defun parse-effect (body)
  ;; returns a disjunction of conjunctions
  (destructuring-bind (disjunctions con) (%parse-effect body)
    (if disjunctions
        (apply #'map-product
               (lambda (&rest args)
                 (reduce #'append args :initial-value con :from-end t))
               (iter (for disjunction in disjunctions)
                     (collect
                      (iter (for disjunction-term in disjunction)
                            (appending (lift-adl disjunction-term))))))
        (list con))))

(defun %parse-effect (body)
  (let (*disjunctions*)
    (let ((conjunction (%%parse-effect body)))
      (list *disjunctions* conjunction))))

(defun %%parse-effect (body)
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
