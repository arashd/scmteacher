;;; printing utils

(define separator1 "====================\n")
(define separator2 "--------------------\n")

(define (block bl)
	(string-append separator2  bl))

(define (answer res)
  (string-append "Answer: \n" (pretty-expression res) "\n"))

(define (answer-block bl)
	(string-append (answer bl) separator2))

