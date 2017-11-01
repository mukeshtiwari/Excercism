#lang racket

(provide my-length
         my-reverse
         my-map
         my-filter
         my-fold
         my-append
         my-concatenate)


(define (my-length l)
  (match l
    ['() 0]
    [(cons h t) (+ 1 (my-length t))]))

(define (my-reverse l)
  (define (my-reverse-inside ls acc)
    (match ls
      ['() acc]
      [(cons h t) (my-reverse-inside t (cons h acc))]))
  (my-reverse-inside l '()))

(define (my-map f l)
  (define (my-map-inside f l acc)
    (match l
      ['() (my-reverse acc)]
      [(cons h t) (my-map-inside f t (cons (f h) acc))]))
  (my-map-inside f l '()))


(define (my-filter f l)
  (define (my-filter-inside f l acc)
    (match l
      ['() (my-reverse acc)]
      [(cons h t) (my-filter-inside f t (if (f h)
                                            (cons h acc)
                                            acc))]))
  (my-filter-inside f l '()))


(define (my-fold f acc l)
  (match l
    ['() acc]
    [(cons h t) (my-fold f (f h acc) t)]))

(define (my-append lone ltwo)
  (define (my-append-inside lone ltwo)
    (match lone
      ['() ltwo]
      [(cons h t) (my-append-inside t (cons h ltwo))]))
  (my-append-inside (my-reverse lone) ltwo))

(define (my-concatenate l)
  (my-fold my-append '() (my-reverse l)))
