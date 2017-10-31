#lang racket

(provide anagrams-for)

(define (string-sort str)
  (list->string (sort (string->list str)  char-ci<?)))

(define (anagrams-for a-str list-string)
  (match list-string
    ['() '()]
    [(cons h t)
     (cond
       [(string=? a-str h) (anagrams-for a-str t)]
       [(string-ci=? (string-sort a-str) (string-sort h)) (cons h (anagrams-for a-str t))]
       [else (anagrams-for a-str t)])]))

