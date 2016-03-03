#lang racket

(provide leap-year?)

(define (leap-year? n)
  (cond
    [(not (= 0 (modulo n 4))) #f]
    [(not (= 0 (modulo n 100))) #t]
    [(not (= 0 (modulo n 400))) #f]
    [else #t]))
