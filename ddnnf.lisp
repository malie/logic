;; depends on some functions from dpll.lisp (cnf-unit-clause


(defun count-atoms-hash-table (c)
  (let ((counts (make-hash-table)))
    (dolist (ds c)
      (dolist (l ds)
        (incf (gethash (abs l) counts 0))))
    counts))

(defvar *prob t)

(defun maxfreq-atom (c)
  (let ((counts (count-atoms-hash-table c)))
    (let (l (n 0))
      (maphash #'(lambda (k v0)
                   (let ((v (+ v0 (if *prob (* (random 10) 0.1) 0))))
                     (when (> v n)
                       (setq n v l k))))
               counts)
      l)))

(defun non-deterministic-or (xs)
  (cond ((null xs)
         t)
        ((and xs (not (cdr xs)))
         (car xs))
        (t
         (cons 'nd-or xs))))

(defun independent (xs)
  `(independent ,@(delete-duplicates (mapcar #'abs xs))))


;; c is a cnf in list of list of disjuncts form;
;; each atom is represented by an integer, negative numbers for negated atoms.
(defun cnf-to-ddnnf (c)
  (cond ((some #'null c)
         nil)
        ((null c)
         t)
        ((null (cdr c))
         (non-deterministic-or (car c)))
        (t
         (let ((ps (find-partitions c)))
           (cond ((and ps (not (null (cdr ps))))
                  ;; (pr 'partition-into (cnfdesc c) (mapcar #'cnfdesc ps))
                  (xcons-ands (mapcar #'cnf-to-ddnnf ps)))
                 (t
                  (let ((l (branch-heuristics c)))
                    (flet ((reca (lit)
                             (multiple-value-bind (cc elim) (xassume lit c)
                               (let ((r (cnf-to-ddnnf cc)))
                                 (cons-xif lit
                                           (cond (elim
                                                  (xcons-and r (independent elim)))
                                                 (t r)))))))
                      (cons-or (reca l)
                               (reca (- l)))))))))))

(defun branch-heuristics (c)
  (or (cnf-unit-clause c)
      (maxfreq-atom c)))

;; http://ipg.host.cs.st-andrews.ac.uk/AI/Lectures/Search4/sld019.htm


;; impose literal on cnf and simplify (a bit)
;; also returns the eliminated variables
(defun xassume (literal cnf)
  (labels ((rec (c res elim nonelim)
             (cond ((null c)
                    (values (reverse res)
                            (remove-duplicates
                             (delete-if #'(lambda (x) (member x nonelim))
                                        elim))))
                   (t
                    (let ((a (car c)))
                      (cond ((member literal a)
                             (rec (cdr c)
                                  res
                                  (append (abslits (remove literal a)) elim)
                                  nonelim))
                            (t
                             (let* ((la (remove (- literal) a)))
                               (rec (cdr c)
                                    (cons la res)
                                    elim
                                    (adjoin-many (abslits la) nonelim))))))))))
    (rec cnf '() '() '())))


(defun abslits (xs)
  (mapcar #'abs xs))

(defun adjoin-many (xs ys)
  (reduce #'(lambda (l x) (adjoin x l))
          xs
          :initial-value ys))

(defun cons-xif (a cs)
  (and cs
       (xcons-and a cs)))

(defun cons-or (a b)
  (cond ((and a b)
         `(or ,a ,b))
        (a a)
        (b b)
        (t nil)))

(defun xcons-ands (xs)
  (reduce #'xcons-and xs))

(defun xcons-and (a b)
  (cond ((or (not a) (not b))
         nil)
        ((eq a t)
         b)
        ((eq b t)
         a)
        ((and (consp a) (eq 'and (car a))
              (consp b) (eq 'and (car b)))
         `(and ,@(cdr a) ,@(cdr b)))
        ((and (consp b) (eq 'and (car b)))
         `(and ,a ,@(cdr b)))
        ((and (consp a) (eq 'and (car a)))
         `(and ,@(cdr a) ,b))
        (t
         `(and ,a ,b))))





(defun make-disjoint-set ()
  (make-hash-table))

(defun ds-add (ds set)
  (let (all0)
    (dolist (x set)
      (let ((s (gethash x ds)))
        (when s
          (push s all0))))
    (let ((all (remove-duplicates (apply #'append set all0))))
      (dolist (x all)
        (setf (gethash x ds) all))))
  ds)

(defun ds-get (ds element)
  (gethash element ds nil))

;; give list of sets, return lists of sets, partitioned on union

(defun find-partitions (c)
  (let ((u (make-disjoint-set)))
    (dolist (xs c)
      (ds-add u (mapcar #'abs xs)))
    ;;(maphash #'(lambda (k v) (print (list k v))) u)
    (let (res)
      (dolist (xs c)
        (let* ((lu (ds-get u (abs (car xs))))
               (l (assoc lu res :test #'eq)))
          (if l
              (push xs (cdr l))
              (setq res (cons (list lu xs) res)))))
      (mapcar #'cdr res))))


(defvar *t nil)

(defun examples ()
  (print (cnf-to-ddnnf '((1 2 3) (3 4 5) (5 6 7))))
  (progn (setq *t (random-cnf 60 264 50 3 3))
         (list *t '===> (time (cnf-to-ddnnf *t))))
  (progn (setq *t (random-cnf 70 304 50 3 3))
         (list *t '===> (time (cnf-to-ddnnf *t))))
  )
