(define-syntax advise-unary
  (syntax-rules ()
    ((advise-unary p wrapper)
     (define p
       (let ((saved-p p)
             (ewrapper wrapper))
         (if (eq? 1 (car (procedure-arity saved-p)))
             (let ((new-p 
                    (named-lambda (p x)
                      (ewrapper 'p saved-p x))))
               (eq-put! new-p 'old-version saved-p)
               new-p)
             (error "not-unary" p)))))))

(define-syntax advise-binary
  (syntax-rules ()
    ((advise-binary p wrapper)
     (define p
       (let ((saved-p p)
             (ewrapper wrapper))
         (if (and (eq? 2 (car (procedure-arity saved-p)))
                  (eq? 2 (cdr (procedure-arity saved-p))))
             (let ((new-p
                    (named-lambda (p x y)
                      (ewrapper 'p saved-p x y))))
               (eq-put! new-p 'old-version saved-p)
               new-p)
             (error "not-binary" p)))))))

(define-syntax advise-ternary
  (syntax-rules ()
    ((advise-ternary p wrapper)
     (define p
       (let ((saved-p p)
             (ewrapper wrapper))
         (if (and (eq? 3 (car (procedure-arity saved-p)))
                  (eq? 3 (cdr (procedure-arity saved-p))))
             (let ((new-p
                    (named-lambda (p x y z)
                      (ewrapper 'p saved-p x y z))))
               (eq-put! new-p 'old-version saved-p)
               new-p)
             (error "not-ternary" p)))))))

(define-syntax advise-sixary
  (syntax-rules ()
    ((advise-ternary p wrapper)
     (define p
       (let ((saved-p p)
             (ewrapper wrapper))
         (if (and (eq? 6 (car (procedure-arity saved-p)))
                  (eq? 6 (cdr (procedure-arity saved-p))))
             (let ((new-p
                    (named-lambda (p x y z a b c)
                      (ewrapper 'p saved-p x y z a b c))))
               (eq-put! new-p 'old-version saved-p)
               new-p)
             (error "not-sixary" p)))))))

(define-syntax advise-nary
  (syntax-rules ()
    ((advise-nary p wrapper)
     (define p
       (let ((saved-p p) (ewrapper wrapper))
         (let ((new-p (begin
                        (named-lambda (p . xs)
                          (ewrapper 'p saved-p xs)))))
           (eq-put! new-p 'old-version saved-p)
           new-p))))))

(define-syntax remove-advice
  (syntax-rules ()
    ((remove-advice p)
     (let ((saved-p (eq-get p 'old-version)))
       (if (not saved-p)
           (warn "unadvised")
           (begin (set! p saved-p)
                  'done))))))