
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
  (push `(:goal
          :parameters ()
          :precondition ,@body
          :effect (and))
        *actions*))

(defmethod process-clause ((clause (eql :metric)) body)
  (setf *metric* body))

;;; process actions

(defun really-process-actions (proto-actions)
  ;; when a negative predicate was found in the precondition, it is added
  ;; to the predicate list, then the entire processing is rerun to ensure
  ;; that all actions consider the effects on the negative predicates.
  (prog (rerun-flag)
    (return
      (restart-bind ((rerun-processing
                      (lambda ()
                        (format t "~%detected negative predicate, restart flag set")
                        (setf rerun-flag t))))
        (let ((result (mappend #'flatten-action proto-actions)))
          (format t "~%Current actions: ~s" result)
          (if rerun-flag
              (progn
                (format t "~%detected negative predicate, restarting")
                (really-process-actions result))
              result))))))

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
             ;; or, exist, imply, forall are compiled into axioms
             (compile-adl-condition `(and ,@type-predicates ,precond))))
       (multiple-value-bind (static-effects conditional-effect-pairs) (parse-effect eff)
         (format t "~& ~a conditional effects in ~a" (length conditional-effect-pairs) name)
         (mapcar (lambda-ematch
                   ((cons precond effects)
                    (list name
                          :parameters param-names
                          :precondition (lift-or `(and ,precond))
                                        ; ensuring being wrapped by AND
                          ;; use the side effect of lift-or: AND is unified
                          :effect (lift-or `(and ,effects)))))
                 (%flatten-conditional-effects simple-precond
                                               static-effects
                                               conditional-effect-pairs)))))))

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
     `(and ,@(enumerate-quantifier params quantified-body)))
    ((list 'exists params quantified-body)
     `(or ,@(enumerate-quantifier params quantified-body)))
    ((list 'imply lhs rhs)
     `(or (not ,lhs) (and ,lhs ,rhs)))
    ((list 'not _)
     (compile-negative-condition condition))
    (_
     condition)))

(defun compile-negative-condition (condition)
  "Compile NOT AND, NOT OR, NOT FORALL, NOT EXISTS, NOT IMPLY to the positive form."
  (ematch condition
    ((list 'not (list* 'and rest))
     (compile-adl-condition `(or ,@(mapcar (lambda (x) `(not ,x)) rest))))
    ((list 'not (list* 'or rest))
     (compile-adl-condition `(and ,@(mapcar (lambda (x) `(not ,x)) rest))))
    ((list 'not (list 'forall params quantified-body))
     (compile-adl-condition `(exists ,params (not ,quantified-body))))
    ((list 'not (list 'exists params quantified-body))
     (compile-adl-condition `(forall ,params (not ,quantified-body))))
    ((list 'not (list 'imply lhs rhs))
     (compile-adl-condition `(not (or (not ,lhs) (and ,lhs ,rhs)))))
    ((list 'not (list* name args))
     (ensure-negative-predicate name args))))

(defun ~ (name)
  (if (negativep name)
      (intern (subseq (symbol-name name) 1) (symbol-package name))
      (symbolicate '~ name)))

(defun negativep (name)
  (char= #\~ (aref (symbol-name name) 0)))

(defun ensure-negative-predicate (name args)
  (let ((~pred (~ name)))
    (or (assoc ~pred *predicates*)
        (progn
          (pushnew (list* ~pred (cdr (assoc name *predicates*))) *predicates* :test #'equal)
          ;; since add/delete effects for negative predicates should be added
          (invoke-restart 'rerun-processing)
          (list* ~pred args)))))

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
                  (pushnew body del :test #'equalp)
                  (when (assoc (~ name) *predicates*)
                    (pushnew `(,(~ name) ,@args) add :test #'equalp)))
                 ((list* name args)
                  (pushnew body add :test #'equalp)
                  (when (assoc (~ name) *predicates*)
                    (pushnew `(not (,(~ name) ,@args)) del :test #'equalp))))))
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
                                                 ,(compile-negative-condition `(not ,condition)))
                                           simple-effects
                                           rest)))
    (nil
     (list (cons simple-precond simple-effects)))))

#+nil
(let ((*types* '((place object) (object)))
      (*objects* '((a . place) (b . place) (c . object))))
  (print
   (flatten-action
    '(move :parameters (?a ?b - place ?c)
      :precondition (and (at ?c ?a) (forall (?d - place) (foo ?d)))
      :effect (and
               (not (at ?c ?a))
               (at ?c ?b)
               (when (bar ?c)
                 (not (bar ?c)))
               (forall (?d - place) (not (foo ?d)))
               (increase (total-cost) 3))))))

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
             ;; or, exist, imply, forall are compiled into axioms
             (compile-adl-condition `(and ,@type-predicates ,condition))))
       `((,name ,param-names)
         ,simple-condition)))))
