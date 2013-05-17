;;; advising to add steps

(define *done* (list '*done*))

;; Index generator
(define (coroutine-index-generator start-index)
  (define (resume-thunk)
    (inc start-index))
  (define (inc index)
    (set! resume-thunk (lambda () (inc (+ index 1))))
    index)
  (lambda () (resume-thunk)))

(define vect-index-gen (coroutine-index-generator 0))
(define (next-vect-name) (string-append "v" (string (vect-index-gen))))

(define matrix-index-gen (coroutine-index-generator 65))
(define (next-matrix-name) 
  (let ((next-matrix-index (matrix-index-gen)))
    (let ((next-char (make-char (+ (remainder (- next-matrix-index 65) 26) 65) 0)))
      (let ((next-subscript (quotient (- next-matrix-index 65) 26)))
        (string-append (string next-char) (string next-subscript))))))

(define (vector-desc v)
  (if (not (null? (get-name v)))
      (string-append "defined as: " (string (get-name v)))
      (next-vect-name)))

(define (matrix-desc m)
  (if (not (null? (get-name m)))
      (string-append "defined as: " (string (get-name m)))
      (next-matrix-name)))

(define (matrix-desc-mutate m)
  (if (not (null? (get-name m)))
      (string-append (string (get-name m)))
      (next-matrix-name)))

(advise-binary vector+vector
               (lambda (name oldf v w)
                 (let ((res-name (next-vect-name))
                       (is-linalg (linalg-element? v)) 
                       (v (convert-to-linalg-element v)) 
                       (w (convert-to-linalg-element w)))
                   (add-step! 
                    (block 
                     (string-append 
                      "Add two vectors:\n" 
                      (vector-desc v) ":\n" 
                      (pretty-expression v) "\nand "
                      (vector-desc w) ":\n" 
                      (pretty-expression w) "\n"
                      "\n\n"
                      "by adding the elements in the same position to get vector " res-name 
                      ".\n\n")))
                   (let ((res (oldf (get-val v) (get-val w))))
                     (end-step! (answer res))
                     (if is-linalg 
                         (create-linalg-element  res-name 'vector res)
                         res)))))

(advise-binary vector-vector
               (lambda (name oldf v w)
                 (let ((res-name (next-matrix-name))
                       (is-linalg (linalg-element? v)) 
                       (v (convert-to-linalg-element v)) 
                       (w (convert-to-linalg-element w)))
                   (add-step! 
                    (block 
                     (string-append 
                      "Subtract two vectors:\n" 
                      (vector-desc v) ":\n" 
                      (pretty-expression v) "\nand "
                      (vector-desc w) ":\n" 
                      (pretty-expression w) "\n"
                      "\n\n"
                      "by subtracting the elements in the same position to get vector " res-name 
                      ".\n\n")))
                   (let ((res (oldf (get-val v) (get-val w))))
                     (end-step! (answer res))
                     (if is-linalg 
                         (create-linalg-element  res-name 'vector res)
                         res)))))

(advise-binary inner-product
               (lambda (name oldf v w)
                 (let ((res-name (next-matrix-name))
                       (v (convert-to-linalg-element v)) 
                       (w (convert-to-linalg-element w)))
                   (let ((w-name (vector-desc w))
                         (v-name (vector-desc v)))
                     (add-step! 
                      (block (string-append 
                              "Calculate the inner product of 2 vectors:\n"
                              v-name "\n"
                              (pretty-expression v) "\nand "
                              w-name ":\n"
                              (pretty-expression w)
                              "\n\n"
                              "by multiplying the elements of vector "
                              v-name
                              " with the elements in the same positions in vector "
                              w-name
                              " and summing the results.\n\n")))
                     (let ((res (oldf (get-val v) (get-val w))))
                       (end-step! (answer res))
                       res)))))

(advise-binary matrix*matrix
               (lambda (name oldf A B)
                 (let ((res-name (next-matrix-name))
                       (is-linalg (linalg-element? A)) 
                       (A (convert-to-linalg-element A))
                       (B (convert-to-linalg-element B)))
                   (let ((B-name (matrix-desc B))
                         (A-name (matrix-desc A)))
                     (add-step! 
                      (block (string-append 
                              "Multiply 2 matrices:\n"
                              A-name ":\n" 
                              (pretty-expression A)
                              "\n and "
                              B-name ":\n"   
                              (pretty-expression B)
                              "\n\n"
                              "by calculating the inner " 
                              "product of each row of the matrix\n"
                              A-name  
                              " and each column of the matrix\n"
                              B-name ".\n\n")))
                     (let ((res (oldf (get-val A) (get-val B))))
                       (end-step! (answer res))
                       (if is-linalg 
                           (create-linalg-element  res-name 'matrix res)
                           res))))))


(advise-binary matrix+matrix
               (lambda (name oldf A B)
                 (let ((res-name (next-matrix-name))
                       (is-linalg (linalg-element? A)) 
                       (A (convert-to-linalg-element A))
                       (B (convert-to-linalg-element B)))
                   (let ((B-name (matrix-desc B))
                         (A-name (matrix-desc A)))
                     (add-step! 
                      (block (string-append 
                              "Adding 2 matrices:\n"
                              A-name ":\n" 
                              (pretty-expression A)
                              "\n and "
                              B-name ":\n"   
                              (pretty-expression B)
                              "\n\n"
                              "by summing up one row at a time.\n\n")))
                     (let ((res (oldf (get-val A) (get-val B))))
                       (end-step! (answer res))
                       (if is-linalg 
                           (create-linalg-element  res-name 'matrix res)
                           res))))))

(advise-binary matrix-matrix
               (lambda (name oldf A B)
                 (let ((res-name (next-matrix-name))
                       (is-linalg (linalg-element? A)) 
                       (A (convert-to-linalg-element A))
                       (B (convert-to-linalg-element B)))
                   (let ((B-name (matrix-desc B))
                         (A-name (matrix-desc A)))
                     (add-step! 
                      (block (string-append 
                              "Subtracting 2 matrices:\n"
                              A-name ":\n" 
                              (pretty-expression A)
                              "\n and "
                              B-name ":\n"   
                              (pretty-expression B)
                              "\n\n"
                              "by subtracting up one row at a time.\n\n")))
                     (let ((res (oldf (get-val A) (get-val B))))
                       (end-step! (answer res))
                       (if is-linalg 
                           (create-linalg-element  res-name 'matrix res)
                           res))))))

(advise-unary row-reduce!
              (lambda (name oldf A)
                (let ((res-name (matrix-desc-mutate A))
                      (is-linalg (linalg-element? A))
                      (M (convert-to-linalg-element A)))
                  (add-step! 
                   (block (string-append 
                           "Row reduce Matrix\n" 
                           (matrix-desc M) 
                           ":\n" (pretty-expression M) "\n"
                           "We can perform three elementary row operations on matrices:\n"
                           "\t1) Multiplying a row by a constant.\n"
                           "\t2) Switching two rows.\n"
                           "\t3) Adding a constant times a row to another row.\n\n")))
                  (let ((res (oldf (get-val M))))
                    (display res)
                    (end-step! (answer M))
                    (if is-linalg 
                        (create-linalg-element  res-name 'matrix (get-val M))
                        res)))))


(advise-unary determinant
              (lambda (name oldf A)
                (let ((res-name (next-matrix-name))
                      (A (convert-to-linalg-element A)))
                  (add-step! 
                   (block (string-append 
                           "Finding the determinant of Matrix\n" 
                           (matrix-desc A)
                           ":\n" (pretty-expression A)
                           "Using Laplace's formula to express the determinant of the matrix in terms of its minors.\n"
                           "The minor Mi,j is defined to be the determinant of the (n−1) × (n−1)-matrix that\n" 
                           "results from A by removing the first row and the jth column. The expression (−1) ^ (1 + j) * M_{1,j} is\n" 
                           "known as a cofactor.\n\n")))
                  (let ((res (oldf (get-val A))))
                    (end-step! (answer res))
                    res))))


(advise-ternary cofactor
                (lambda (name oldf v i A)
                  (let ((A (convert-to-linalg-element A)))
                    (add-step! 
                     (block 
                      (string-append 
                       "Multiplying the " (string i) "th element in the first row by the determinant of its Minor:" "\n" 
                       (pretty-expression A) "\n"
                       "to get a cofactor\n\n")))
                    (let ((res (oldf v i (get-val A))))
                      (end-step! (answer (string-append "Resulting cofactor: " (string res))))
                      res))))

(advise-ternary find-pivot
                (lambda (name oldf A row col)
                  (let ((A (convert-to-linalg-element A)))
                    (add-step! 
                     (block 
                      (string-append 
                       "Find the largest pivot element in column: " (string (+ 1 col)) "\n" 
                       "from row: " (string (+ 1 row)) " to the bottom of the column\n"
                       "in Matrix:\n"
                       (pretty-expression A)
                       "and swap up the row.\n\n"
                       )))
                    (let ((res (oldf (get-val A) row col)))
                      (end-step! (answer (string-append "row: " (string (+ 1 res)) " col: " (string (+ 1 col)))))
                      res))))

(advise-ternary vector-swap!
                (lambda (name oldf v i j)
                  (let ((res-name (next-matrix-name))
                        (v (convert-to-linalg-element v))
                        (is-linalg (linalg-element? v)))
                    (add-step! 
                     (block 
                      (string-append 
                       "Swap two rows " (string (+ 1 j) " and " (string (+ 1 i)) " of the matrix:\n" 
                                                (pretty-expression v)) "\n\n")))
                    (let ((res (oldf (get-val v) i j)))
                      (end-step! (answer v))
                      (if is-linalg 
                          (create-linalg-element  res-name 'matrix res)
                          res)))))

(advise-sixary add-row!
               (lambda (name oldf A alpha row1 row2 col col-end)
                 (let ((res-name (next-matrix-name))
                       (A (convert-to-linalg-element A))
                       (is-linalg (linalg-element? A)))
                   (add-step! 
                    (block 
                     (string-append 
                      "Add " (string alpha) " times of row " (string (+ 1 row1)) "\n" 
                      "to row " (string (+ 1 row2)) "\n"
                      "in Matrix:\n"
                      (pretty-expression A) "\n\n")))
                   (let ((res (oldf (get-val A) alpha row1 row2 col col-end)))
                     (end-step! (answer A))
                     (if is-linalg 
                         (create-linalg-element  res-name 'matrix res)
                         res)))))

(advise-nary + 
             (lambda (name oldf xs)
               (cond ((for-all? xs linalg-matrix?) 
                      (let* ((x (car xs))
                             (x-row-len (vector-length (get-val x)))
                             (x-col-len (vector-length (vector-ref (get-val x) 0))))
                        (fold-left matrix+matrix (car xs) (cdr xs))))
                     ((for-all? xs linalg-vector?) 
                      (let* ((x (car xs))
                             (x-row-len (vector-length (get-val x))))
                        (fold-left vector-vector (car xs) (cdr xs))))
                     (else (if (= (length xs) 1) (oldf (car xs)) (fold-left oldf (car xs) (cdr xs)))))))

(advise-nary - 
             (lambda (name oldf xs)
               (cond ((for-all? xs linalg-matrix?) 
                      (let* ((x (car xs))
                             (x-row-len (vector-length (get-val x)))
                             (x-col-len (vector-length (vector-ref (get-val x) 0))))
                        (fold-left matrix-matrix (car xs) (cdr xs))))
                     ((for-all? xs linalg-vector?) 
                      (let* ((x (car xs))
                             (x-row-len (vector-length (get-val x))))
                        (fold-left vector-vector (car xs) (cdr xs))))
                     (else (if (= (length xs) 1) (oldf (car xs)) (fold-left oldf (car xs) (cdr xs)))))))

(advise-nary *
             (lambda (name oldf xs)
               (cond ((for-all? xs linalg-element?)
                      (let* ((elements (map (lambda (element) 
                                              (if (linalg-vector? element) 
                                                  (create-matrix (make-vector 1 (get-val element))) 
                                                  element))
                                            xs))
                             (x (car elements))
                             (x-row-len (vector-length (get-val x)))
                             (x-col-len (vector-length (vector-ref (get-val x) 0))))
                        (fold-left matrix*matrix (car elements) (cdr elements))))
                     (else (if (= (length xs) 1) (oldf (car xs)) (fold-left oldf (car xs) (cdr xs)))))))

(advise-unary display
              (lambda (name oldf x)
                (if (linalg-element? x) 
                    (oldf (pretty-expression x))
                    (oldf x))))
