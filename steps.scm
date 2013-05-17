(define steps '())
(define steps-graph (create-graph))
(define step-depth 0)
(define current-node '())
(define shown-node '())
(define root-node '())

(define (print-data n)
  (display (node-data n)))

(define (init-graph!)
  (set! steps-graph (create-graph))
  (let ((first-node (add-node steps-graph "Starting Computation\n")))
    (set! current-node first-node)
    (set! shown-node first-node)
    (set! root-node first-node)))

(define (init? input)
  (if (not (pair? input))
      #f
      (not (memq (car input) '(show-steps backup)))))

(define (add-step! msg)
  (let ((sn (add-node steps-graph msg)))
    (add-child current-node sn)
    (set! current-node sn)))


(define (end-step! answer-msg)
  (append-node-data! current-node answer-msg)
  (set! current-node (parent current-node)))

(define (level)
  (define (level-aux sf str)
    (if (= sf step-depth)
        str
        (level-aux (+ sf 1) (string-append "-" str))))
  (level-aux 0 ""))

(define (dfs tree fn #!optional max-depth)
  (define (dfs-aux sub-tree depth)
    (if (or (default-object? max-depth) (< depth max-depth))
        (begin
          (fn sub-tree)
          (for-each (lambda (st) (dfs-aux st (+ 1 depth))) (children sub-tree)))))
  (dfs-aux tree -1))  ; -1 because we're starting from one level above root

(define (show-all-steps #!optional max-depth)
  (display separator1)
  (if (null? root-node) (warn "No computations to show steps for.\n")
      (dfs root-node print-data max-depth))
  (display separator1))

(define (show-steps #!optional step-num)
  (display separator1)
  (if (default-object? step-num)
      (begin
        (dfs shown-node print-data 1)
        (if (null? (children shown-node)) (display "No further steps to show.\n")))
      (begin
        (if (eq? shown-node root-node) (set! shown-node (car (children shown-node))))
        (if (> step-num (length (children shown-node))) (warn "No such step.\n")
            (begin
              (set! shown-node (list-ref (children shown-node) (- step-num 1)))
              (show-steps)))))
  (display separator1))

(define (backup)
  (let ((p (parent shown-node)))
    (if (null? p) (display "Can't backup any further.\n")
        (set! shown-node p)))
  (display "backed up\n"))
