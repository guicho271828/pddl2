
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
        *ground-functions*)
    (iter (for (c-kind . c-body) in body)
          (funcall #'process-clause c-kind c-body))
    (let ((*print-length* 100))
      (print (list
              *init*
              *goal*
              *ground-predicates*
              *ground-actions*
              *ground-axioms*
              *ground-functions*)))))

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
        (let ((body (append (untype-parameters *objects*) body)))
          (values (remove '= body :key #'first)
                  (mapcar #'cdr
                          (remove '= body :key #'first :test (complement #'eq))))))
  (iter (for gf in *ground-functions*)
        (assert (match gf
                  ((list _ (number)) t))
                nil
                "object fluents in ~a !" gf)))

(defun untype-parameters (params)
  (iter outer
        (for (p . type) in params)
        (for types = (assoc type *types*))
        (iter (for type in types)
              (in outer
                  (collect `(,type ,p))))))

(defvar *disjunctions*)

(defmethod process-clause ((clause (eql :goal)) body)
  (setf *goal* (lift-adl body)))

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
  (destructuring-bind (disjunctions con) (%%lift-adl body)
    (if disjunctions
        (apply #'map-product
               (lambda (&rest args)
                 (reduce #'append args :initial-value con :from-end t))
               (iter (for disjunction in disjunctions)
                     (collect
                      (iter (for disjunction-term in disjunction)
                            (appending (lift-adl disjunction-term))))))
        (list con))))

(defun %%lift-adl (body)
  (let (*disjunctions*)
    (let ((conjunction (%lift-adl body)))
      (list *disjunctions* conjunction))))

(defun %lift-adl (body)
  (match body
    ((list* 'and rest)
     (mappend #'%lift-adl rest))
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
     (%lift-adl `(or ,@(mapcar (lambda (x) `(not ,x)) rest))))
    ((list 'not (list* 'or rest))
     (mappend #'%lift-adl rest))
    ((list 'not (list* 'forall params quantified-body))
     (%lift-adl `(exists ,params (not ,quantified-body))))
    ((list 'not (list* 'exists params quantified-body))
     (%lift-adl `(forall ,params (not ,quantified-body))))
    ((list 'not (list* 'imply lhs rhs))
     ;; t t -> t -> n
     ;; t n -> n -> t
     ;; n t -> t -> n
     ;; n n -> t -> n
     (%lift-adl `(and ,lhs (not ,rhs))))
    (_
     (list body))))




         #+nil
         (appendf *actions*
                  (ematch body
                    ((list* :parameters params
                            :precondition precond
                            :effect eff)
                     (let* ((params (parse-typed-list params t))
                            (type-precond (type-predicates params)))
                       (canonicalize-action params `(and ,@type-precond ,precond) eff)))))

(defmethod process-clause ((clause (eql :metric)) body)
  (assert (match body
            ((list 'minimize (list 'total-cost)) t))
          nil
          "We do not support costs other than total-cost!"))

#+nil
(defmethod process-clause ((clause (eql :requirements)) body)
  )
