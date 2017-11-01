#lang racket

(provide convert)

(define (div-theorem n v)
  (zero? (remainder n v)))

(define (remove-all-v n v)
  (cond
    [(div-theorem n v) (remove-all-v (quotient n v) v)]
    [else n]))

(define (convert-friend n acc)
  (cond
    [(div-theorem n 3) (convert-friend (remove-all-v n 3) (string-append acc "Pling"))]
    [(div-theorem n 5) (convert-friend (remove-all-v n 5) (string-append acc "Plang"))]
    [(div-theorem n 7) (convert-friend (remove-all-v n 7) (string-append acc "Plong"))]
    [else acc]))


(define (convert t)
  (let ([acc (convert-friend t "")])
    (cond
      [(string=? acc "") (number->string t)]
      [else acc])))