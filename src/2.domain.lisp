
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
(defvar *types* nil
  "An alist of (type . supertypes).
 SUPERTYPES is a list containing _ALL_ supertypes of the type. E.g.
  *types* = ((truck . (car object))) == ((truck car object)).
 It should contain (OBJECT).")
(defvar *objects* nil
  "An alist of (OBJECT-NAME . TYPE). TYPE is a symbol designating the type e.g. (truck0 . truck)")
(defvar *predicates*)
(defvar *numeric-fluents*)
(defvar *object-fluents*)
(defvar *actions*)
(defvar *axioms*)

(defun parse-domain (body)
  "Parse the domain body as a list. It generally ignores the requirements."
  (let ((*requirements* '(:strips))
        (*types* '((object)))
        *objects*
        *predicates*
        *numeric-fluents*
        *object-fluents*
        *axioms*
        *actions*)
    (iter (for (c-kind . c-body) in body)
          (funcall #'process-clause c-kind c-body))
    (let ((*print-length* 8))
      (print (list
              *requirements*
              *types*
              *objects*
              *predicates*
              *numeric-fluents*
              *object-fluents*
              *axioms*
              *actions*
              *current-pathname*)))))

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

(defun variablep (token)
  (char= #\? (aref (symbol-name token) 0)))

(defun parse-typed-list (body &optional check-variable (default 'object))
  (let (acc tmp)
    (iter (generate token in body)
          (next token)
          (if (eq '- token)
              (let ((super (next token)))
                (check-type super (and symbol (not null)))
                (iter (for token in tmp)
                      (push (cons token super) acc))
                (setf tmp nil))
              (progn
                (when check-variable
                  (assert (variablep token) nil
                          "the name of symbol ~a does not start with #\? despite being variable" token))
                (push token tmp)))
          (finally
           (iter (for token in tmp)
                 (push (cons token default) acc))))
    acc))

(defun collect-indirect-type-relationship (alist)
  (prog ()
    :start
    (return
      (let* ((types (remove-duplicates (cons 'object (mapcar #'car alist))))
             (len (length types))
             (a (make-array (list len len) :element-type 'boolean :initial-element nil)))
        (iter (for (type . super) in alist)
              (unless (position type types)
                (warn "Defining an undeclared type ~a as a subtype of OBJECT" type)
                (push (cons super 'object) alist)
                (go :start))
              (unless (position super types)
                (warn "Defining an undeclared supertype ~a of type ~a" super type)
                (push (cons super 'object) alist)
                (go :start))
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
        (iter (for type in types)
              (for i = (position type types))
              (collect (cons type
                             (iter (for j from 0 below len)
                                   (when (aref a i j)
                                     (collect (elt types j)))))))))))

;;; constants

(defmethod process-clause ((clause (eql :constants)) body)
  (setf *objects*
        (parse-typed-list body)))

;;; predicates

(defmethod process-clause ((clause (eql :predicates)) body)
  (appendf *predicates*
           (mapcar (lambda-ematch
                     ((list* name args)
                      (cons name (mapcar #'cdr (parse-typed-list args t)))))
                   body)))

;;; numeric fluents

(defmethod process-clause ((clause (eql :functions)) body)
  (assert (null *numeric-fluents*))
  (assert (null *object-fluents*))
  (setf (values *numeric-fluents* *object-fluents*)
        (iter (for elem in (parse-typed-list body))
              (match elem
                ((cons (list* name args) 'number)
                 (collect (cons name (mapcar #'cdr (parse-typed-list args t))) into nf))
                ((cons (list* name args) 'object)
                 (collect (cons name (mapcar #'cdr (parse-typed-list args t))) into of)))
              (finally
               (return (values nf of))))))

(defmethod process-clause ((clause (eql :fluents)) body)
  (process-clause :functions body))

;;; actions

(defmethod process-clause ((clause (eql :action)) body)
  ;; it does not process actions meaningfully. it is diferred until grounding.
  (push body *actions*))

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
