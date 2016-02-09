
(in-package :pddl2.impl)

(defun read-pddl (file)
  (with-input-from-file (s file)
    (macroexpand (read s))))


