;;; package
(in-package :pddl2.impl)

(defun map-plist (fn plist)
  (iter (for (key value . rest) on plist by #'cddr)
        (collect (funcall fn key value))))

(defun make-trie (list)
  (let (acc (list (remove-duplicates list :test #'equal)))
    (iter (for (head . rest) in list)
          (push rest (getf acc head)))
    ;; (sleep 1)
    (map-plist (lambda (head children)
                 (if (equalp children '(()))
                     (list head)
                     (cons head (make-trie children))))
               acc)))

;; (defun make-trie (list)
;;   `(+root+ ,@(%make-trie list)))

(defun merge-trie (t1 t2)
  (let ((acc (copy-tree t1)))
    (iter (for subtrie2 in t2)
          (for (head . rest) = subtrie2)
          (if-let ((subtrie1 (assoc head acc)))
            (setf (cdr subtrie1)
                  (merge-trie (cdr subtrie1) rest))
            (push subtrie2 acc)))
    acc))

(defun trie= (t1 t2)
  (iter (for subtrie2 in t2)
        (for (head . rest) = subtrie2)
        (always
         (when-let ((subtrie1 (assoc head t1)))
           (trie= (cdr subtrie1) rest)))))

(defun trie-member (list trie)
  (match list
    ((cons head nil)
     (match (assoc head trie)
       ((cons _ nil)
        t)))
    ((cons head rest)
     (match (assoc head trie)
       ((cons _ rest2)
        (trie-member rest rest2))))))
