
(in-package :pddl2.impl)

(defvar *current-filename*)

(defun read-pddl (*current-filename*)
  (format t "~&; loading ~a~&" *current-filename*)
  (with-input-from-file (s *current-filename*)
    (eval (read s))))

(lisp-namespace:define-namespace domain)
(lisp-namespace:define-namespace problem)

(defmacro define ((category name) &body body)
  (ematch category
    ('domain
     `(setf (symbol-domain ',name) (parse-domain ',body)))
    ('problem
     `(setf (symbol-problem ',name) (parse-problem ',body)))))


