#lang racket

(provide hamming-distance)

(define (hamming-distance-solver first second)
  (let ([xs (string->list first)]
        [ys (string->list second)])
    (count (negate equal?) xs ys)))
  
  

(define (hamming-distance first second)
  (cond
    [(eq? (string-length first) (string-length second)) (hamming-distance-solver first second)]
    [else (error "not equal length strings")]))
