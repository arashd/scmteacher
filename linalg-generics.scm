(define pretty-expression
  (make-generic-operator 1 (lambda (exp) (string exp))))

(define convert-to-linalg-element
  (make-generic-operator 1 (lambda (x) (error "wrong arg to convert"))))

(defhandler pretty-expression (lambda (exp) (pretty-matrix (get-val exp))) 
  linalg-matrix?)

; ========

(defhandler pretty-expression (lambda (exp) (pretty-vector (get-val exp)))
  linalg-vector?)

(defhandler pretty-expression (lambda (exp) (pretty-vector exp)) vector?)

(defhandler pretty-expression (lambda (exp) (pretty-matrix exp)) vect-of-vect?) 

(defhandler pretty-expression (lambda (exp) (string exp)) number?)

(defhandler pretty-expression (lambda (exp) exp) string?)

; ========

(defhandler convert-to-linalg-element
  (lambda (x) 
    (create-linalg-element '() 'vector x)) vector?)

(defhandler convert-to-linalg-element
  (lambda (x) 
    (create-linalg-element '() 'matrix x)) vect-of-vect?)

(defhandler convert-to-linalg-element
  (lambda (x) x) linalg-element?)

; ========

