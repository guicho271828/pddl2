
(in-package :pddl2.impl)

(defmacro -> (&body body)
  (compile--> (reverse body)))

(defun compile--> (rbody)
  (match rbody
    ((list form)
     form)
    ((list* (list* head args) rest)
     (with-gensyms (tmp)
       `((lambda (,tmp) (,head ,tmp ,@args))
         ,(compile--> rest))))))
