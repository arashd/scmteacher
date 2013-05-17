(define (matrix mat) (cdr mat))

; (define (row-ref mat i)
;   (vector-ref (matrix mat) i))

; (define (column-ref mat j) 
;   (vector-map (lambda (v) (vector-ref v j)) (matrix mat)))

(define (row-len mat) (vector-length (row-ref mat 1)))
(define (column-len mat) (vector-length (matrix mat)))

(define create-vector 
  (make-generic-operator 1))

(define create-matrix
  (make-generic-operator 1))

(defhandler create-matrix
  (lambda (mat)
    (if (call-with-current-continuation
         (lambda (return)
           (if (not (list? mat)) (return #f)
               (let ((row-len (length (car mat))))
                 (for-each
                  (lambda (elem)
                    (if (not (list? elem)) (return #f) 
                        (if (not (eq? (length elem) row-len)) (return #f)))) mat)))))
        (create-linalg-element '() 'matrix (list->vector (map list->vector mat)))
        (invalid-matrix-syntax)))
  list?)

(defhandler create-matrix
  (lambda (mat) 
    (create-matrix (vector->list (vector-map vector->list mat)))) 
  vector?)


(defhandler create-vector 
  (lambda (l) (create-linalg-element '() 'vector  
                                     (list->vector l))) 
  list?)

(defhandler create-vector 
  (lambda (l) (create-linalg-element '() 'vector
                                     l)) 
  vector?)

(define (rand-matrix m n)
  (create-matrix 
    (tabulate-matrix (lambda (i j) (random 100)) m n)))


(define (rand-vector l)
  (create-vector 
    (tabulate-vector (lambda (i) (random 100)) l)))


(define (invalid-matrix-syntax)
  (warn "invalid syntax in matrix definition"))


(define (vect-of-vect? exp)
  (and (vector? exp) (for-all? (vector->list exp) vector?)))

(define (linalg-matrix? exp)
  (and (linalg-element? exp) (eq? (get-type exp) 'matrix)))

(define (linalg-vector? exp)
  (and (linalg-element? exp) (eq? (get-type exp) 'vector)))


;;; Macro to loop over integers from n (inclusive) to m (exclusive)
(define-syntax for
  (syntax-rules ()
    ((for (i n m) forms ...)
     (let ((fixed-m m))
       (let loop ((i n))
         (if (< i fixed-m)
             (begin
               forms ...
               (loop (+ i 1)))))))))

;;; Loop over exactly the same range as for (for ...), but do it
;;; backwards
(define-syntax reverse-for
  (syntax-rules ()
    ((for (i n m) forms ...)
     (let ((fixed-n n))
       (let loop ((i (- m 1)))
         (if (>= i fixed-n)
             (begin
               forms ...
               (loop (- i 1)))))))))

(define-syntax unless
  (syntax-rules ()
    ((unless condition body ...)
     (if (not condition) (begin body ...)))))

;;; Tabulate a procedure in a vector
(define (tabulate-vector proc size)
  (let ((vec (make-vector size)))
    (for (i 0 size)
         (vector-set! vec i (proc i)))
    vec))

;;; Vector analog of `map'
(define (map-vector proc v)
  (tabulate-vector (lambda (i) (proc (vector-ref v i)))
                   (vector-length v)))

;;; Copy a vector
(define (copy-vector v)
  (vector-map (lambda (x) x) v)) 

;;; Copy a matrix
(define (copy-matrix m)
  (vector-map copy-vector m))

(define (matrix-ref matrix row col)
  (vector-ref (vector-ref matrix row) col))

(define (matrix-set! matrix row col value)
  (vector-set! (vector-ref matrix row) col value))

(define (tabulate-matrix proc num-rows num-cols)
  (tabulate-vector
   (lambda (i)
     (tabulate-vector (lambda (j) (proc i j)) num-cols))
   num-rows))

(define (transpose-matrix m)
  (let ((num-rows (vector-length m))
        (num-cols (vector-length (vector-ref m 0))))
    (tabulate-matrix (lambda (i j) (matrix-ref m j i))
                     num-cols num-rows)))

;;; Make the i-th unit vector of length n.
(define (unit-vector i n)
  (tabulate-vector (lambda (j) (if (= i j) 1 0)) n))

;;; Make the identity matrix of dimension n
(define (identity-matrix n)
  (tabulate-vector (lambda (i) (unit-vector i n)) n))

;;; Make the zero matrix
(define (zero-matrix m n)
  (make-vector m (make-vector n 0)))

;;; Make the identity permutation of length n
(define (identity-permutation n)
  (tabulate-vector (lambda (i) i) n))

;;; Apply a permutation to a vector.
;;; If the vector is also a permutation, then this is
;;; permutation multiplication.
(define (apply-permutation perm v)
  (tabulate-vector (lambda (i)
                     (vector-ref v (vector-ref perm i)))
                   (vector-length perm)))

;;; Swap two elements in a vector.
;;; Works OK if i = j.
(define (vector-swap! v i j)
  (let ((vi (vector-ref v i))
        (vj (vector-ref v j)))
    (vector-set! v i vj)
    (vector-set! v j vi)))

;;; Find an optimal pivot in matrix m.
;;; Search in column col, from row to bottom of the column.
(define (find-pivot m row col)
  (let ((best-row row)
        (best-value (abs (matrix-ref m row col))))
    (for (r (+ row 1) (vector-length m))
         (let ((value (abs (matrix-ref m r col))))
           (if (> value best-value)
               (begin
                 (set! best-row r)
                 (set! best-value value)))))
    best-row))

;;; Find an optimal pivot and perform the swap,
;;; also on the permutation.
(define (perform-pivot! perm m row col)
  (let ((pivot (find-pivot m row col)))
    (vector-swap! m row pivot)
    (vector-swap! perm row pivot)))

(define (perform-reduced-pivot! perm m row col)
  (let ((pivot (find-pivot m row col)))
    (vector-swap! m row pivot)))

;;; Add alpha*row1 to row2, starting from column col to col-end
(define (add-row! m alpha row1 row2 col col-end)
  (for (c col col-end)
       (matrix-set! m row2 c
                    (+ (matrix-ref m row2 c)
                       (* alpha
                          (matrix-ref m row1 c))))))

;;; Eliminate row2 by using row1
(define (eliminate-row! m row1 row2 col col-end)
  (let ((alpha (/ (matrix-ref m row2 col) 
                  (matrix-ref m row1 col))))
    (add-row! m (- alpha) row1 row2 col col-end)
    ; (matrix-set! m row2 col alpha)
    ))

; eliminate row-from to row-to (exclusively) using row
(define (eliminate-rows! m row row-from row-to col col-end)
  (for (i row-from row-to)
    (eliminate-row! m row i col col-end)))

;;; Make LU factorisation without pivoting
(define (lu-factor! m)
  (let ((size (vector-length m)))
    (for (col 0 (- size 1))
      (for (row (+ col 1) size)
        (eliminate-row! m col row col size)))))

;;; Make LU factorisation with pivoting.
;;; Return the permutation.
(define (lu-factor/pivoting! m)
  (let* ((size (vector-length m))
         (perm (identity-permutation size)))
    (for (col 0 (- size 1))
      (perform-pivot! perm m col col)
      (for (row (+ col 1) size)
        (eliminate-row! m col row col size)))
    perm))

;;; Scale row with factor alpha starting from column col to col-end
(define (scale-row! m alpha row col col-end)
  (for (c col col-end)
    (matrix-set! m row c
                 (* alpha (matrix-ref m row c)))))


; perform the wanted type of row reduction on the matrix m,
; return the list of pivot columns
(define (perform-row-reduction! m echelon-form? reduced?)
  (let* ((row-end   (vector-length m))
         (col-end   (vector-length (vector-ref m 0)))
         (perm      (identity-permutation row-end))
         (pivot-row 0)
         (pivots    '()))
    (for (col 0 col-end)
      (unless (= pivot-row row-end)
        (perform-reduced-pivot! perm m pivot-row col)
        (unless (= (matrix-ref m pivot-row col) 0)
          (set! pivots (cons col pivots))
          (if echelon-form?  ; the pivots are ones in echelon form
              (scale-row! m (/ (matrix-ref m pivot-row col)) pivot-row col col-end))
          (eliminate-rows! m pivot-row (+ pivot-row 1) row-end col col-end)
          (if reduced?       ; in reduced echolon from the pivot is the only non-zero element
              (eliminate-rows! m pivot-row 0 pivot-row col col-end))
          (set! pivot-row (+ pivot-row 1)))))
    (reverse pivots)))

(define (row-reduce! m)
  (perform-row-reduction! m #f #f))

(define (row-echelon-form! m)
  (perform-row-reduction! m #t #f))

(define (reduced-row-echelon-form! m)
  (perform-row-reduction! m #t #t))

;;; Compute a "partial inproduct", i.e. an inproduct of a slice
;;; of both vectors, indicated by [n,m)
(define (partial-inproduct v1 v2 n m)
  (let loop
      ((i n)
       (sum 0))
    (if (= i m)
        sum
        (loop (+ i 1) (+ sum (* (vector-ref v1 i)
                                (vector-ref v2 i)))))))


;;; Solve the system L x = b, for some lower-triangle matrix L
;;; with 1's on the diagonal. Only the part of l under the diagonal
;;; is actually inspected, so the rest can contain arbitrary data.
;;; (E.g. the U part of the LU decomposition which generated L.)
(define (solve-lower-triangle l b)
  (let* ((x-len (vector-length l))
         (x (make-vector x-len)))
    (for (i 0 x-len)
      (vector-set! x i
                   (- (vector-ref b i)
                      (partial-inproduct
                       (vector-ref l i) x
                       0 i))))
    x))

;;; Solve the system U x = b, for some upper-triangle matrix u.
;;; Only the part of u up and above the diagonal
;;; is actually inspected, so the rest can contain arbitrary data.
;;; (E.g. the L part of the LU decomposition which generated U.)
(define (solve-upper-triangle u b)
  (let* ((x-len (vector-length u))
         (x (make-vector x-len)))
    (reverse-for (i 0 x-len)
                 (vector-set! x i
                              (/ (- (vector-ref b i)
                                    (partial-inproduct
                                     (vector-ref u i) x
                                     (+ i 1) x-len))
                                 (matrix-ref u i i))))
    x))

;;; Return a "solver procedure" for the matrix.
;;; The result can be used to find the solution in some point, e.g.
;;; (define solver (make-solver test-matrix))
;;; (display (solver some-vector))
(define (make-solver m)
  (let ((lu (copy-matrix m)))
    (lu-factor! lu)
    (lambda (v)
      (solve-upper-triangle lu (solve-lower-triangle lu v)))))

;;; Return a "solver procedure" for the matrix, with pivoting.
(define (make-solver/pivoting m)
  (let* ((lu   (copy-matrix m))
         (perm (lu-factor/pivoting! lu)))
    (lambda (v)
      (let ((v (apply-permutation perm v)))
        (solve-upper-triangle lu 
                              (solve-lower-triangle lu v))))))

;;; Compute the inverse of a matrix.
;;; Do NOT use this procedure if all you want to do is solving
;;; a system Ax = b. In that case, use make-solver/pivoting.
(define (invert-matrix m)
  (let ((solver (make-solver/pivoting m))
        (unit   (identity-matrix (vector-length m))))
    (transpose-matrix (vector-map solver unit))))

(define (matrix->smatrix m)
  (list->vector (map list->vector m)))

(define (smatrix->matrix sm)
  (map vector->list (vector->list sm)))

(define (inner-product v w)
  (fold-left + 0 (vector->list (vector-map * v w))))

(define dot inner-product)

(define (norm-squared v)
  (inner-product v v))

(define (norm v)
  (sqrt (norm-squared v)))

(define (scale-vector s v)
  (vector-map (lambda (vi) (* s vi)) v))

(define (vector+vector v w)
  (vector-map + v w))

(define (vector-vector v w)
  (vector-map - v w))

(define (matrix*matrix A B)
  (let ((A-row-len   (vector-length (vector-ref A 0)))
        (B-col-len   (vector-length B)))
    (if (not (= A-row-len B-col-len))
        (error "Incorrect matrix dimensions: ")
        (vector-map (lambda (Ai)
                      (vector-map (lambda (Bj)
                                    (inner-product Ai Bj))
                                  (transpose B))) A))))

(define (matrix+matrix A B)
  (let ((A-row-len (vector-length A))
        (A-col-len (vector-length (vector-ref A 0)))
        (B-row-len (vector-length B))
        (B-col-len (vector-length (vector-ref B 0))))
      (if (or (not (= A-row-len B-row-len)) (not (= A-col-len B-col-len)))
          (error "Incorrect matrix dimensions: ")
          (vector-map vector+vector A B))))

; (define (matrix-matrix A B)
;   (matrix+matrix A (vector-map (lambda (v) (scale-vector -1 v)) B)))

(define (matrix-matrix A B)
  (let ((A-row-len (vector-length A))
        (A-col-len (vector-length (vector-ref A 0)))
        (B-row-len (vector-length B))
        (B-col-len (vector-length (vector-ref B 0))))
      (if (or (not (= A-row-len B-row-len)) (not (= A-col-len B-col-len)))
          (error "Incorrect matrix dimensions: ")
          (vector-map vector-vector A B))))

(define (append-vector v w)
  (list->vector
    (append 
      (vector->list v)
      (vector->list w))))

(define (determinant A)
  (let ((A-row-len  (vector-length A))
        (A-col-len  (vector-length (vector-ref A 0))))
    (if (not (= A-row-len A-col-len)) (error "Not square matrix")
      (if (= A-row-len 1) 
          (matrix-ref A 0 0)
          (let ((first-row (vector-first A))
                (perm (identity-permutation A-row-len)))
            (fold-left + 0
              (vector->list
                (vector-map
                  (lambda (v i) (cofactor v i (remove-row-col A 0 i)))
                  first-row perm))))))))

(define (cofactor v i A)
  (* (expt (- 1) (+ 1 (+ i 1))) v (determinant A)))

(define (remove-row-col A r c)
  (let ((A-r (append-vector (subvector A 0 r) (subvector A (+ r 1) (vector-length A)))))
    (vector-map (lambda (v) 
                  (append-vector
                    (subvector v 0 c)
                    (subvector v (+ c 1) (vector-length v)))) A-r)))

(define (pretty-vector v)
  (fold-left
   (lambda (sf i) 
     (string-append sf (string #\tab) (string i))) 
   (string)
   (vector->list v)))

(define (pretty-matrix m)
  (fold-left
   (lambda (sf v) 
     (string-append sf (pretty-vector v) (string #\newline))) 
   (string)
   (vector->list m)))

(define (transpose m)
  (transpose-matrix m))
