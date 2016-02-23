
(in-package :pddl2.impl)

(defvar *current-pathname* nil)

(defun read-pddl (*current-pathname*)
  (format t "~&; loading ~a~&" *current-pathname*)
  (with-input-from-file (s *current-pathname*)
    (eval (read s))))

(lisp-namespace:define-namespace domain)
(lisp-namespace:define-namespace problem)

(defmacro define ((category name) &body body)
  (ematch category
    ('domain
     `(setf (symbol-domain ',name) (parse-domain ',body)))
    ('problem
     `(setf (symbol-problem ',name) (parse-problem ',body)))))


