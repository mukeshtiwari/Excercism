#lang racket

(provide nucleotide-counts)

(define (n-count c nuc)
  (count (lambda (x) (eq? x c)) (string->list nuc)))

(define (valid-char c)
  (cond
    [(or (eq? c #\A) (eq? c #\C)
         (eq? c #\G) (eq? c #\T)) #t]
    [else #f]))

(define (valid-string str)
  (andmap valid-char (string->list str)))

(define (nucleotide-counts str)
  (if (valid-string str)
      (map (lambda (c) (cons c (n-count c str))) (list #\A #\C #\G #\T))
      (raise (exn:fail))))