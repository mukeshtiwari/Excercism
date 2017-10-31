#lang racket

(provide collatz)

(define (collatz-acc w k)
       (cond
         [(eq?  w 1) k]
         [(even? w) (collatz-acc (quotient w 2) (+ k 1))]
         [else (collatz-acc (+ 1 (* 3 w)) (+ k 1))]))

(define (collatz n)
  (cond
    [(and (integer? n) (> n 0)) (collatz-acc n 0)]
    [else (error "Unwanted cases")]))

