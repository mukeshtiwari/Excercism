#lang racket

(provide etl)


(define (etl-friend x y)
  (map (lambda (z)
         (cond
           [(< x 0) (raise (exn:fail))]
           [else (cons (string-downcase z) x)])) y))

(define (etl given-hash)
  (make-hash
   (apply append
          (hash-map given-hash etl-friend))))
