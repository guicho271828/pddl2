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

(defun merge-trie (&optional t1 t2)
  (if (and t1 t2)
      (%nmerge-trie (copy-tree t1) t2)
      (or t1 t2)))

(defun nmerge-trie (&optional t1 t2)
  (if (and t1 t2)
      (%nmerge-trie t1 t2)
      (or t1 t2)))

(defun %nmerge-trie (t1 t2)
  (iter (for subtrie2 in t2)
        (for (head . rest) = subtrie2)
        (if-let ((subtrie1 (assoc head t1)))
          (setf (cdr subtrie1)
                (%nmerge-trie (cdr subtrie1) rest))
          (push subtrie2 t1)))
  t1)

(defun trie-subsetp (t1 t2)
  "t1 is a subset of t2"
  (iter (for subtrie1 in t1)
        (for (head . rest) = subtrie1)
        (always
         (when-let ((subtrie2 (assoc head t2)))
           (trie-subsetp rest (cdr subtrie2))))))

(defun trie-equal (t1 t2)
  (and (trie-subsetp t1 t2)
       (trie-subsetp t2 t1)))

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

(defun map-trie (fn trie)
  (labels ((rec (path current)
             (iter (for (head . subtrie) in current)
                   (if subtrie
                       (rec (cons head path) subtrie)
                       (funcall fn (reverse (cons head path)))))))
    (rec nil trie)))

(defun pop-trie (trie)
  (let (result)
    (labels ((rec (path trie)
               (ematch trie
                 ((list* (cons head nil) rest)
                  (setf result (cons head path))
                  rest)
                 ((list* (cons head subtrie) rest)
                  (let ((new-subtrie (rec (cons head path) subtrie)))
                    (if new-subtrie
                        (list* (cons head new-subtrie) rest)
                        rest))))))
      (let ((r2 (rec nil trie)))
        (values (nreverse result)
                r2)))))

(defun %trie1 (list)
  (match list
    (nil
     nil)
    ((cons head nil)
     (list head))
    ((cons head rest)
     (list head (%trie1 rest)))))

(defun %push-trie (thing trie)
  "destructively modifies trie to insert a list THING"
  (labels ((rec (thing trie)
             (ematch thing
               (nil
                nil)
               ((cons head rest)
                (if-let ((subtrie (assoc head trie)))
                  (rec rest (cdr subtrie))
                  (push (%trie1 thing) (cdr trie)))))))
    (if trie
        (progn (rec thing trie) trie)
        (list (%trie1 thing)))))

(defmacro push-trie (thing place)
  `(setf ,place (%push-trie ,thing ,place)))
