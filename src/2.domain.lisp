
;;; package
(in-package :pddl2.impl)

(defgeneric process-clause (clause body)
  (:method (clause body)
    (error "unsupported feature ~a in: ~a" clause body)))

(defun parse-clause (body clause &optional (optional t))
  "Parse the body for the specified clause. When optional = nil and the clause was not found, signal an error."
  (match body
    ((list* (list* (eq clause) clause-body) rest)
     (funcall #'process-clause clause clause-body)
     rest)
    (_
     (if optional
         body
         (error "~@<missing a required clause ~a in ~a~@:>" clause body)))))

;; It does not trust any of the metadata defined in the sections.
(defvar *requirements*)
(defvar *types*)
(defvar *objects*)
(defvar *predicates*)
(defvar *functions*)
(defvar *actions*)
(defvar *axioms*)

(defun parse-domain (body)
  "Parse the domain body as a list. It generally ignores the requirements."
  (let ((*requirements* '(:strips))
        *types*
        *objects*
        *predicates*
        *functions*
        *axioms*
        *actions*)
    (iter (for (c-kind . c-body) in body)
          (funcall #'process-clause c-kind c-body))
    (print (list
            *types*
            *objects*
            *predicates*
            *functions*
            *axioms*
            *actions*))))

;;; requirement

(defmethod process-clause ((clause (eql :requirements)) body)
  (when-let ((unsupported (intersection body '(:durative-actions
                                               :timed-initial-literals
                                               :preferences
                                               :constraints))))
    (error "unsupported requirements! ~a " unsupported)))

;;; types

(defmethod process-clause ((clause (eql :types)) body)
  (push :typing *requirements*)
  (setf *types*
        (-> body
            (parse-typed-list)
            (collect-indirect-type-relationship)))
  (setf *predicates*
        (iter (for type in (remove 'object (mapcar #'first *types*)))
              (collect `(,type object)))))

(defun parse-typed-list (body &optional check-variable)
  (let (acc tmp)
    (iter (generate token in body)
          (next token)
          (check-type token (and symbol (not null)))
          (if (eq '- token)
              (let ((super (next token)))
                (check-type super (and symbol (not null)))
                (iter (for type in tmp)
                      (push (cons type super) acc)
                      #+nil
                      (push super (getf acc type)))
                (setf tmp nil))
              (progn
                (when check-variable
                  (assert (char= #\? (aref (symbol-name token) 0))))
                (push token tmp)))
          (finally
           (iter (for type in tmp)
                 (push (cons type 'object) acc)
                 #+nil
                 (push 'object (getf acc type)))))
    acc))

(defun collect-indirect-type-relationship (alist)
  (let* ((types (remove-duplicates (cons 'object (mapcar #'car alist))))
         (len (length types))
         (a (make-array (list len len) :element-type 'boolean :initial-element nil)))
    (iter (for (type . super) in alist)
          (unless (aref a
                        (position type types)
                        (position super types))
            (setf (aref a
                        (position type types)
                        (position super types))
                  t)))
    (iter (with flag = nil)
          (iter (for i below len)
                (iter (for j below len)
                      (iter (for k below len)
                            (when (and (aref a i j) (aref a j k)
                                       (not (aref a i k)))
                              (setf (aref a i k) t
                                    flag t)))))
          (while flag)
          (setf flag nil))
    (iter (for type in (remove 'object types))
          (for i = (position type types))
          (collect (cons type
                         (remove
                          'object
                          (iter (for j from 0 below len)
                                (when (aref a i j)
                                  (collect (elt types j))))))))))

;;; constants

(defmethod process-clause ((clause (eql :constants)) body)
  (setf *objects*
        (mapcar (lambda-ematch
                  ((cons object type)
                   (cons object (cdr (assoc type *types*)))))
                (parse-typed-list body))))

;;; predicates

(defmethod process-clause ((clause (eql :predicates)) body)
  (setf *predicates*
        (mapcar (lambda-ematch
                  ((list* name args)
                   (cons name (mapcar #'cdr (parse-typed-list args t)))))
                body)))

;;; numeric fluents

(defmethod process-clause ((clause (eql :functions)) body)
  (assert (null *functions*))
  (setf *functions*
        (mapcar (lambda-ematch
                  ((list* name args)
                   (cons name (mapcar #'cdr (parse-typed-list args t)))))
                body)))

(defmethod process-clause ((clause (eql :fluents)) body)
  (assert (null *functions*))
  (setf *functions*
        (mapcar (lambda-ematch
                  ((list* name args)
                   (cons name (mapcar #'cdr (parse-typed-list args t)))))
                body)))

;;; actions

(defmethod process-clause ((clause (eql :action)) body)
  ;; it does not process actions meaningfully. it is diferred until grounding.
  (push body *actions*)
  #+nil
  (appendf *actions*
           (ematch body
             ((list* :parameters params
                     :precondition precond
                     :effect eff)
              (let* ((params (parse-typed-list params t))
                     (type-precond
                      (iter outer
                            (for (p . type) in params)
                            (for types = (assoc type *types*))
                            (iter (for type in types)
                                  (in outer
                                      (collect `(,type ,p)))))))
                (canonicalize-action params `(and ,@type-precond ,precond) eff))))))

(defmethod process-clause ((clause (eql :derived)) body)
  ;; (:derived (blocked-trans ?p - process ?t - transition) (and ...))
  (push body *axioms*))

;; A predicate P is called derived if there is a rule that has a predicate P in
;; its head; otherwise P is called basic. The restrictions that apply are:
;; 
;; 1. The actions available to the planner do not affect the derived predicates: no
;; derived predicate occurs in any of the effect lists of the domain actions.
;; 
;; 2. If a rule indicates that P (\bar{x}) can be derived from φ(\bar{x}), then the variables in \bar{x} are
;; pairwise different (and, as the notation suggests, the free variables of φ(\bar{x}) are
;; exactly the variables in \bar{x}, or a subset of them).
;; 
;; 3. If a rule indicates that P (\bar{x}) can be derived from φ, then the Negation Normal
;; Form (NNF) of φ(x) does not contain any derived predicates in negated form.
