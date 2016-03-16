(in-package :pddl2.impl)

(defun log-logger ()
  (let ((c 1) (log 0) prev)
    (lambda (what)
      (if (eq prev what)
          (progn
            (incf c)
            (let ((log-new (floor (log c 2))))
              (when (< log log-new)
                (format t "~&; ~ath consequtive ~a~%" c what)
                (setf log log-new))))
          (progn
            (format t "~&; switched to ~a~%" what)
            (setf prev what c 1 log 0))))))

(defun bind-action (action binding)
  "binding : (param . obj)"
  (ematch* (action binding)
    (((list name
           :parameters params
           :precondition precond
           :effect eff)
      (cons parameter object))
     (assert (member parameter params))
     (list name
           :parameters (remove parameter params)
           :precondition (subst object parameter precond)
           :effect (subst object parameter eff)))))

(defun nbind-action (action binding)
  "binding : (param . obj)"
  (ematch* (action binding)
    (((list _
           :parameters (place params)
           :precondition (place precond)
           :effect (place eff))
      (cons parameter object))
     (assert (member parameter params))
     (setf params  (delete parameter params)
           precond (nsubst object parameter precond)
           eff     (nsubst object parameter eff))
     action)))

(defun bind-action1 (action object)
  "bind the first parameter"
  (ematch action
    ((list name
           :parameters (list* parameter rest)
           :precondition precond
           :effect eff)
     (list name
           :parameters rest
           ;; :precondition (subst-condition object parameter precond)
           ;; :effect (subst-condition object parameter eff)
           :precondition (subst object parameter precond)
           :effect (subst object parameter eff)
           ))))

(defun nbind-action1 (action object)
  "bind the first parameter: destructive"
  (ematch action
    ((list _
           :parameters (place params)
           :precondition (place precond)
           :effect (place eff))
     (setf precond (nsubst object (car params) precond)
           eff     (nsubst object (car params) eff)
           params  (cdr params))
     action)))


(defun check-action (action reachable)
  "Returns T if the action is applicable to the reachable sets of states, if lifted variables are ignored"
  (labels ((check-condition (condition)
             (ematch condition
               ((list* 'and conditions)
                (every #'check-condition conditions))
               ((list* 'or conditions)
                (some #'check-condition conditions))
               ((list 'not (list '= x y))
                (if (not (or (variablep x) (variablep y)))
                    (not (eq x y))
                    t))
               ((list '= x y)
                (if (not (or (variablep x) (variablep y)))
                    (eq x y)
                    t))
               ((list 'not _)
                t)
               ((list* head params)
                (test-parameter head params (cdr (assoc head reachable)))))))
    (ematch action
      ((list* _
              :parameters _
              :precondition precond _)
       (check-condition precond)))))

(defun test-parameter (head params obj-trie)
  "test if the given list of parameters is have a possible binding"
  (ematch params
    (nil t)
    ((list* p ps)
     (if (variablep p)
         (iter (for (obj . subtrie) in obj-trie)
               (thereis
                (test-parameter head ps subtrie)))
         (when-let ((found (assoc p obj-trie)))
           (test-parameter head ps (cdr found)))))))

(defun ~test-parameter (head params obj-trie)
  ;; no longer in use, since checking the negative predicate is not necessary
  "test if the given list of parameters have a possible binding --- for negative predicates"
  (ematch params
    (nil nil)
    ((list* p ps)
     (if (variablep p)
         (or (iter (for (obj . type) in *objects*)
                   (thereis
                    (not (assoc obj obj-trie))))
             (iter (for (obj . subtrie) in obj-trie)
                   (thereis
                    (~test-parameter head ps subtrie))))
         (if-let ((found (assoc p obj-trie)))
           (~test-parameter head ps (cdr found))
           t)))))

