#lang racket

(provide sum-of-squares square-of-sums difference)

;;(n * (n + 1) / 2)
(define (square-of-sums n)
  (sqr (/ (* n (+ n 1)) 2)))

(define (sum-of-squares n)
  (/ (* n (+ n 1) (+ 1 (* 2 n))) 6))

(define (difference n)
  (- (square-of-sums n) (sum-of-squares n)))
  