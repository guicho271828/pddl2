
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

(defmacro do-restart (bindings &body body)
  "A construct that, after a restart is invoked, it jumps to the start and reevaluate
the body. Example:

 (do-restart ((retry (lambda (c) (print :retry)))
           (continue (lambda (c) (print :retry))))
   (error \"error!\"))
"
  (with-gensyms (start)
    `(block nil
       (tagbody
          ,start
          (return
            (restart-bind
                ,(mapcar
                  (lambda (binding)
                    (destructuring-bind
                          (name function . key-value-pair)
                        binding
                      (with-gensyms (rest)
                        `(,name (named-lambda ,(concatenate-symbols name 'handler)
                                    (&rest ,rest)
                                  (prog1
                                      (apply ,function ,rest)
                                    (go ,start)))
                                ,@key-value-pair))))
                  bindings)
              ,@body))))))
