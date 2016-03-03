#lang racket
(provide square total)

(define (iterate n x f)
  (if (= n 1)
      x
      (iterate (- n 1) (f x) f)))

(define (square n)
  (iterate n 1 (λ(x) (* x 2))))

(define (total)
  (apply + (map square (range 1 65))))