
(in-package :pddl2.impl)

(defun read-pddl (file)
  (with-input-from-file (s file)
    (macroexpand (read s))))

(lisp-namespace:define-namespace domain)
(lisp-namespace:define-namespace problem)

(defmacro define ((category name) &body body)
  (ematch category
    ('domain
     (setf (symbol-domain name) (parse-domain body)))
    ('problem
     (setf (symbol-problem name) (parse-problem body)))))


