#lang racket

(provide to-roman)

; This map is to make sure that I am subtracting
; values correctly. 
(define num-map
  #hash((1000 . "M")
        (900 . "CM")
        (500 . "D")
        (400 . "CD")
        (100 . "C")
        (90 . "XC")
        (50 . "L")
        (49 . "IL")
        (40 . "XL")
        (10 . "X")
        (9 . "IX")
        (5 . "V")
        (4 . "IV")
        (1 . "I")))


(define (to-roman-acc n acc)
  (cond
    [(zero? n) acc]
    [(>= n 1000) (to-roman-acc (- n 1000) (string-append acc "M"))]
    [(>= n 900) (to-roman-acc (- n 900) (string-append acc "CM"))]
    [(>= n 500) (to-roman-acc (- n 500) (string-append acc "D"))]
    [(>= n 400) (to-roman-acc (- n 400) (string-append acc "CD"))]
    [(>= n 100) (to-roman-acc (- n 100) (string-append acc "C"))]
    [(>= n 90) (to-roman-acc (- n 90) (string-append acc "XC"))]
    [(>= n 50) (to-roman-acc (- n 50) (string-append acc "L"))]
    [(>= n 49) (to-roman-acc (- n 49) (string-append acc "IL"))]
    [(>= n 40) (to-roman-acc (- n 40) (string-append acc "XL"))]
    [(>= n 10) (to-roman-acc (- n 10) (string-append acc "X"))]
    [(>= n 9) (to-roman-acc (- n 9) (string-append acc "IX"))]
    [(>= n 5) (to-roman-acc (- n 5) (string-append acc "V"))]
    [(>= n 4) (to-roman-acc (- n 4) (string-append acc "IV"))]
    [else (to-roman-acc (- n 1) (string-append acc "I"))]))
    
     
(define (to-roman n)
  (to-roman-acc n ""))