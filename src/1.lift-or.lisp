;;; package
(in-package :pddl2.impl)

(defpattern op (x)
  `(and ,x (or 'and 'or)))

(defun make-condition-binary (condition)
  (match condition
    ((list (op _))
     condition)
    ((list (op _) y)
     (make-condition-binary y))
    ((list (op x) y z)
     (list x
           (make-condition-binary y)
           (make-condition-binary z)))
    ((list* (op x) y z rest)
     (make-condition-binary
      (list* x (list x
                     (make-condition-binary y)
                     (make-condition-binary z))
             rest)))
    (_ condition)))

(defun disjunction-conjunction (condition)
  (ematch condition
    ((list 'and (list 'or x y) z)
     `(or ,(disjunction-conjunction
            `(and ,x ,z))
          ,(disjunction-conjunction
            `(and ,y ,z))))
    ((list 'and z (list 'or x y))
     `(or ,(disjunction-conjunction
            `(and ,x ,z))
          ,(disjunction-conjunction
            `(and ,y ,z))))
    ((list 'or x y)
     `(or ,(disjunction-conjunction x)
          ,(disjunction-conjunction y)))
    ((list 'and x y)
     (multiple-value-match (disjunction-conjunction x)
       ((_ t)
        (multiple-value-match (disjunction-conjunction y)
          ((_ t)
           (values `(and ,x ,y) t))
          ((y nil)
           (disjunction-conjunction `(and ,x ,y)))))
       ((x nil)
        (disjunction-conjunction `(and ,x ,y)))))
    (_ (values condition t))))

(defun flatten-condition (condition)
  (match condition
    ((list* (op op) rest)
     (let ((subexp (mapcar #'flatten-condition rest)))
       (let* ((same (remove-if-not
                     (lambda-match
                       ((list* (eq op) _)
                        t))
                     subexp))
              (diff (remove-if
                     (lambda-match
                       ((list* (eq op) _)
                        t))
                     subexp)))
         `(,op ,@(mappend #'cdr same) ,@diff))))
    (_ (values condition t))))

(defun lift-or (condition)
  (flatten-condition
   (disjunction-conjunction
    (make-condition-binary condition))))
