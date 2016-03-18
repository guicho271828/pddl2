
;;; package
(in-package :pddl2.impl)

(defvar *domain-name*)

(defvar *init*)
(defvar *metric*)

(defvar *ground-numeric-fluents*)
(defvar *ground-object-fluents*)

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
        *domain-name*
        *init*
        *metric*
        *ground-numeric-fluents*
        *ground-object-fluents*)
    (iter (for (c-kind . c-body) in body)
          (funcall #'process-clause c-kind c-body))
    ;; removing disjunctive conditions
    (setf *actions* (really-process-actions *actions*))
    (setf *axioms*  (really-process-axioms *axioms*))
    ;; grounding actions, axioms and functions
    (let ((*print-length* 8))
      (print (list *requirements*
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
                   *current-pathname*)))))

;;; process clauses
(defmethod process-clause ((clause (eql :domain)) body)
  (ematch body
    ((list domain-name)
     (setf *domain-name* domain-name
           (values *requirements*
                   *types*
                   *objects*
                   *predicates*
                   *numeric-fluents*
                   *object-fluents*
                   *axioms*
                   *actions*)
           (values-list (symbol-domain domain-name))))))

(defmethod process-clause ((clause (eql :objects)) body)
  ;; apppend to the constants
  (appendf *objects* (parse-typed-list body)))

(defmethod process-clause ((clause (eql :init)) body)
  (setf (values *ground-numeric-fluents*
                *ground-object-fluents*
                *init*)
        (iter (for elem in body)
              (ematch elem
                ((list '= place (and value (number)))
                 (collect (append place (list value)) into nf))
                ((list '= place (and value (symbol)))
                 (collect (append place (list value)) into of))
                (_
                 (collect elem into init)))
              (finally
               (return (values nf of
                               (append init (types-as-predicates *objects*))))))))

(defun types-as-predicates (params)
  (prog ()
    :start
    (return
      (iter outer
            (for (p . type) in params)
            (for types = (assoc type *types*))
            (unless types
              (warn "Defining an undeclared type ~a as a subtype of OBJECT" type)
              (push (list type 'object) *types*)
              (go :start))
            (iter (for type in types)
                  (when (eq type 'object)
                    (next-iteration))
                  (in outer
                      (collect `(,type ,p))))))))

(defmethod process-clause ((clause (eql :goal)) body)
  (push `(*goal*
          :parameters ()
          :precondition ,@body
          :effect (and))
        *actions*))

(defmethod process-clause ((clause (eql :metric)) body)
  (setf *metric* body))

;;; process actions

(defun really-process-actions (proto-actions)
  (mappend #'flatten-action proto-actions))

(defun flatten-action (proto-action)
  "Compile OR, NOT, FORALL, EXISTS, IMPLY, WHEN"
  (ematch proto-action
    ((list name
           :parameters params
           :precondition precond
           :effect eff)
     (let* ((params (parse-typed-list params t))
            (param-names (mapcar #'car params))
            (type-predicates (types-as-predicates params))
            (simple-precond
             ;; or, exists, imply, forall are compiled into axioms
             (compile-adl-condition `(and ,@type-predicates ,precond))))
       (multiple-value-bind (static-effects conditional-effect-pairs) (parse-effect eff)
         (format t "~& ~a conditional effects in ~a" (length conditional-effect-pairs) name)
         (mapcar (lambda-ematch
                   ((cons precond effects)
                    (list name
                          :parameters param-names
                          :precondition (lift-or2 `(and ,precond))
                                        ; ensuring being wrapped by AND
                          ;; use the side effect of lift-or: AND is unified
                          :effect (lift-or2 `(and ,effects)))))
                 (%flatten-conditional-effects simple-precond
                                               static-effects
                                               conditional-effect-pairs)))))))

(defun free-variables (condition)
  (match condition
    ((list* (or 'and 'or 'imply 'not) rest)
     (reduce #'union (mapcar #'free-variables rest)))
    ((list (or 'exists 'forall) params quantified-body)
     (set-difference (free-variables quantified-body)
                     (mapcar #'first (parse-typed-list params t))))
    ((list* _ params)
     (remove-if-not #'variablep params))))

(defun enumerate-quantifier (params quantified-body)
  "Compiles the body of FORALL and EXISTS"
  (let* ((params (parse-typed-list params t))
         (matched (iter (for (p . type) in params)
                        (collect
                            (iter (for (o . ot) in *objects*)
                                  (for supers = (assoc ot *types*))
                                  (when (member type supers)
                                    (collect o)))))))
    (apply #'map-product
           (lambda (&rest args)
             (iter (with body = quantified-body)
                   (for p in (mapcar #'first params))
                   (for o in args)
                   (setf body (subst o p body))
                   (finally (return body))))
           matched)))

(defun compile-adl-condition (condition)
  "compiles FORALL, EXISTS, IMPLY and its negations into grounded AND and OR."
  (match condition
    ((list* 'and rest)
     `(and ,@(mapcar #'compile-adl-condition rest)))
    ((list* 'or rest)
     `(or ,@(mapcar #'compile-adl-condition rest)))
    ((list 'forall params quantified-body)
     (compile-adl-condition
     `(and ,@(enumerate-quantifier params quantified-body))))
    ((list 'exists params quantified-body)
     (compile-adl-condition
     `(or ,@(enumerate-quantifier params quantified-body))))
    ((list 'imply lhs rhs)
     `(or (not ,lhs) (and ,lhs ,rhs)))
    ((list 'not condition)
     ;; push negation inwards
     (negate-condition condition))
    ((list* (or 'not 'forall 'exists 'imply) _)
     (error "syntax error in ~a" condition))
    (_
     condition)))

(defun negate-condition (condition)
  (ematch condition
    ((list* 'and rest)
     (compile-adl-condition `(or ,@(mapcar (lambda (x) `(not ,x)) rest))))
    ((list* 'or rest)
     (compile-adl-condition `(and ,@(mapcar (lambda (x) `(not ,x)) rest))))
    ((list 'forall params quantified-body)
     (compile-adl-condition `(exists ,params (not ,quantified-body))))
    ((list 'exists params quantified-body)
     (compile-adl-condition `(forall ,params (not ,quantified-body))))
    ((list 'imply lhs rhs)
     (compile-adl-condition `(not (or (not ,lhs) (and ,lhs ,rhs)))))
    (_
     `(not ,condition))))

(defun parse-effect (body)
  "Extract WHEN, compile FORALL, and flatten AND tree."
  (let (conditional-effect-pairs fluents add del)
    (labels ((rec (body)
               (match body
                 ((or (list 'not (list* (or 'and 'or 'forall 'exists 'imply) _))
                      (list* 'exists _))
                  (error "invalid effect: ~a" body))
                 ((list* 'and rest)
                  (map nil #'rec rest))
                 ((list 'forall params quantified-body)
                  (map nil #'rec (enumerate-quantifier params quantified-body)))
                 ((list 'when condition effect)
                  (let ((simple-condition
                         (compile-adl-condition condition)))
                    (multiple-value-bind (simple-effect
                                          more-condition-effect-pairs)
                        (parse-effect effect)
                      (assert (null more-condition-effect-pairs)
                              nil
                              "WHEN cannot contain further WHEN: (WHEN <GD> <COND-EFFECT>")
                      (push (cons simple-condition simple-effect)
                            conditional-effect-pairs))))
                 ((list* 'when _)
                  (error "syntax error in ~a" body))
                 ((list (or 'assign 'increase 'decrease 'scale-up 'scale-down) _ _)
                  (push body fluents))
                 ((list* (or 'assign 'increase 'decrease 'scale-up 'scale-down) _)
                  (error "syntax error in ~a" body))
                 ((list 'not (list* name args))
                  (pushnew body del :test #'equalp))
                 ((list* name args)
                  (pushnew body add :test #'equalp)))))
      (rec body)
      (values `(and ,@del ,@add ,@fluents)
              conditional-effect-pairs))))

(defun %flatten-conditional-effects (simple-precond simple-effects conditional-effect-pairs)
  (ematch conditional-effect-pairs
    ((list* (cons condition effects) rest)
     (append (%flatten-conditional-effects `(and ,simple-precond ,condition)
                                           `(and ,simple-effects ,effects)
                                           rest)
             (%flatten-conditional-effects `(and ,simple-precond
                                                 ,(negate-condition `(not ,condition)))
                                           simple-effects
                                           rest)))
    (nil
     (list (cons simple-precond simple-effects)))))

;;; process axioms

(defun really-process-axioms (proto-actions)
  (mappend #'flatten-axiom proto-actions))

(defun flatten-axiom (proto-axiom)
  "Compile OR, NOT, FORALL, EXISTS, IMPLY, WHEN"
  (ematch proto-axiom
    ((list (list* name params) condition)
     (let* ((params (parse-typed-list params t))
            (param-names (mapcar #'car params))
            (type-predicates (types-as-predicates params))
            (simple-condition
             ;; or, exists, imply, forall are compiled into axioms
             (compile-adl-condition `(and ,@type-predicates ,condition))))
       `((,name ,param-names)
         ,simple-condition)))))
