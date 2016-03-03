#lang racket

(provide perfect-numbers)

(define (sum-of-factors n)
  (let* ([factors (filter (λ(x) (= 0 (modulo n x))) (range 2 (sqrt n)))]
        [final-sum (foldl (λ(v acc) (+ acc v (/ n v))) 1 factors)])
        final-sum))

(define (perfect-numbers n)
  (filter (λ(x) (= x (sum-of-factors x))) (range 2 (+ n 1))))
