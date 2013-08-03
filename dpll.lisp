(declaim (optimize (speed 3) (space 0) (safety 1) (debug 0)))
;;(declaim (optimize (speed 0) (space 0) (safety 3) (debug 3)))



(defun random-from-to (min max)
  (+ min (random (+ 1 (- max min)))))

(defun random-clause (numvars numliterals)
  (let ((res (make-hash-table)))
    (do ()
        ((= (hash-table-count res) numliterals))
      (setf (gethash (random-from-to 1 numvars) res) t))
    (let (reslist)
      (maphash #'(lambda (k v) (declare (ignore v))
                         (push k reslist))
               res)
      (sort reslist #'<))))

(defun random-cnf (numvars numclauses percentnegated minliterals maxliterals)
  (let (res)
    (dotimes (i numclauses)
      (let ((clause (mapcar #'(lambda (a) (if (< (random 100) percentnegated) (- a) a))
                            (random-clause numvars (random-from-to minliterals maxliterals)))))
        (push clause res)))
    res))




(defun count-literals-hash-table (c)
  (let ((counts (make-hash-table)))
    (dolist (ds c)
      (dolist (l ds)
        (incf (gethash l counts 0))))
    counts))

(defun maxfreq-literal (c)
  (let ((counts (count-literals-hash-table c)))
    (let (l (n 0))
      (maphash #'(lambda (k v)
                   (when (> v n)
                     (setq n v l k)))
               counts)
      l)))

(defvar *verbose t)
(defun pr (&rest xs)
  (when *verbose
    (print xs)
    (terpri)))

(defun solve (c)
  (cond ((some #'null c)
         (pr 'some-clause-went-nil)
         nil)
        ((null c)
         (pr 'no-more-clauses)
         '(t))
        ((null (cdr c))
         (pr 'single-clause-left (car c))
         (car c))
        (t
         (let ((un (cnf-unit-clause c)))
           (cond (un
                  (let ((cc (assume un c)))
                    (pr 'propagate-unit-clause un (cnfdesc cc))
                    (let ((res (solve cc)))
                      (when res
                        (cons un res)))))
                 (t
                  (let ((resolution-atom (resolution-candidate c)))
                    (cond (resolution-atom
                           (let ((rest (apply-resolution c resolution-atom)))
                             (pr 'apply-resolution resolution-atom (cnfdesc rest))
                             (solve rest)))
                          (t
                           (let ((l (maxfreq-literal c)))
                             (pr 'assume-maxfreq-literal l)
                             (let ((cc (assume l c)))
                               (pr 'assuming l '==> (cnfdesc cc))
                               (let ((res (solve cc)))
                                 (cond (res
                                        (cons l res))
                                       (t
                                        (pr 'assume-maxfreq-literal-negated (- l))
                                        (let ((cc (assume (- l) c)))
                                          (pr 'assuming (- l) '==> (cnfdesc cc))
                                          (let ((res (solve cc)))
                                            (when res
                                              (cons (- l) res))))))))))))))))))

(defun cnfdesc (c)
  (let ((nc (length c)))
    (if (> nc 10)
        (list 'num-clauses nc
              'num-literals (length (apply #'append c))
              'distinct-atoms (length (remove-duplicates (mapcar #'abs (apply #'append c)))))
        c)))

(defun cnf-unit-clause (c)
  (let (res)
    (dolist (ds c)
      (when (null (cdr ds))
        (return-from cnf-unit-clause (car ds))))))

(defun assume (literal cnf)
  (and cnf
       (let ((a (car cnf)))
         (cond ((member literal a)
                (assume literal (cdr cnf)))
               (t
                (cons (remove (- literal) a)
                      (assume literal (cdr cnf))))))))


;; (-a | X) & (a | Y)   ===>  X | Y
;; no other occurences of -a or a
(defun apply-resolution (cnf atom)
  (labels ((res (cnf res resolvent)
             (cond (cnf
                    (let ((ds (car cnf)))
                      (cond ((or (member atom ds)
                                 (member (- atom) ds))
                             (res (cdr cnf)
                                  res
                                  (append resolvent (remove (- atom) (remove atom ds)))))
                            (t
                             (res (cdr cnf) (cons ds res) resolvent)))))
                   (t
                    (values res resolvent)))))
    (multiple-value-bind (res resolvent)
        (res cnf nil nil)
      (cons resolvent res))))


(defun resolution-candidate (c)
  (let ((counts (count-literals-hash-table c)))
    (let (l (n 0))
      (maphash #'(lambda (k v)
                   (when (and (equal 1 v)
                              (equal 1 (gethash (- k) counts)))
                     (return-from resolution-candidate (abs k))))
               counts)
      nil)))


(defun compare-clauses (a b)
  (cond ((and (consp a) (consp b))
         (let ((aa (car a)) (bb (car b)))
           (cond ((eq aa bb)
                  (compare-clauses (cdr a) (cdr b)))
                 ((null aa)  -1)
                 ((null bb)  1)
                 ((< aa bb) -1)
                 (t         1))))
        ((null a)  -1)
        ((null b)  1)))


(defun unique-cnf (cnf)
  (drop-successive-dups (sort (mapcar #'(lambda (ds) (sort ds #'<)) cnf)
                              #'compare-clauses)))

(defun drop-successive-dups (list)
  (cond ((cdr list)
         (let ((a (car list))
               (b (cadr list)))
           (cond ((equal a b)
                  (drop-successive-dups (cdr list)))
                 (t
                  (cons a (drop-successive-dups (cdr list)))))))
        (t
         list)))


(defun one-of (vars)
  (cons vars
        (pairwise-excludes vars)))

(defun pairwise-excludes (xs)
  (and xs
       (let ((l (car xs))
             res)
         (dolist (x (cdr xs))
           (push (list (- l) (- x)) res))
         (concatenate 'list res (pairwise-excludes (cdr xs))))))

(defun sudoku-var (column line digit)
  (assert (<= 0 column 8))
  (assert (<= 0 line 8))
  (assert (<= 0 digit 8))
  (+ (* column 81)
     (* line 9)
     digit
     1))

(defun sudoku-coords (var)
  (multiple-value-bind (col r) (floor (- var 1) 81)
    (multiple-value-bind (lin dig) (floor r 9)
      (values col lin dig))))

(defun sudoku-field (col line)
  (let (res)
    (dotimes (num 9)
      (push (sudoku-var col line num) res))
    res))

(defun sudoku-line (line num)
  (let (res)
    (dotimes (x 9)
      (push (sudoku-var x line num) res))
    res))

(defun sudoku-col (col num)
  (let (res)
    (dotimes (y 9)
      (push (sudoku-var col y num) res))
    res))

(defun sudoku-block (r l num)
  (let (res)
    (dotimes (x 3)
      (dotimes (y 3)
        (push (sudoku-var (+ (* 3 r) x) (+ (* 3 l) y) num) res)))
    res))

(defun sudoku ()
  (let (res)
    (dotimes (line 9)
      (dotimes (col 9)
        (push (one-of (sudoku-field col line)) res)))
    (dotimes (line 9)
      (dotimes (num 9)
        (push (one-of (sudoku-line line num)) res)))
    (dotimes (col 9)
      (dotimes (num 9)
        (push (one-of (sudoku-col col num)) res)))
    (dotimes (br 3)
      (dotimes (bl 3)
        (dotimes (num 9)
          (push (one-of (sudoku-block br bl num)) res))))
    (apply #'append res)))

(defun sudoku-game (fields)
  (let ((col 0) (line 0) cl res)
    (dolist (f fields)
      (when (integerp f)
        (push (list (sudoku-var col line (- f 1))) res))
      (incf col)
      (when (= col 9)
        (setq col 0)
        (incf line)))
    res))

(defun solve-game (givens)
  (let* ((gvs (sudoku-game givens))
         (_ (print `(givens ,gvs)))
         (cnf0 (append (sudoku) gvs))
         (cnf (unique-cnf cnf0))
         (res0 (solve cnf))
         (res (delete t res0)))
    (print (mapcan #'(lambda (x) (and (< 0 x) (list x))) res))
    (terpri)
    (flet ((on (x y num)
             (let ((v (sudoku-var x y num)))
               (or (member v res)
                   (member (list v) gvs :test #'equal)))))
      (dotimes (lines 9)
        (dotimes (cols 9)
          (princ "  ")
          (dotimes (num 9)
            (when (on cols lines num)
              (princ (+ 1 num))))
          (princ ""))
        (terpri)))))

(defun some-game ()
  (solve-game
   '(1 _ _ _ _ 5 3 _ 7
     _ 9 2 1 4 _ _ _ _
     _ _ _ 8 _ _ _ _ _
     _ _ _ _ 7 _ 5 _ 1
     _ 4 _ _ _ _ _ 7 _
     5 _ 6 _ 8 _ _ _ _
     _ _ _ _ _ 6 _ _ _
     _ _ _ _ 3 9 2 5 _
     2 _ 9 4 _ _ _ _ 3)))


;; pure literal elimination (no -x against x, no x against -x)
