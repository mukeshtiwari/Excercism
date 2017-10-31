#lang racket
(require racket/hash)
 
(provide word-count)



(define (combine-hash-maps f s)
  (hash-union f s #:combine/key (lambda (k u v) (+ u v))))

(define (make-list-hash str)
  (map make-immutable-hash
       (map (lambda (x) (cons (cons x 1) '()))
            (filter non-empty-string? (string-split str  #rx"\n| ")))))

(define (word-count word-string)
  (foldl combine-hash-maps (make-immutable-hash) (make-list-hash word-string)))

