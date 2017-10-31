#lang racket

(provide to-rna)


(define (rna-map c)
  (match c
    [#\G #\C]
    [#\C #\G]
    [#\T #\A]
    [#\A #\U]
    [else (error "undefined value")]))

(define (to-rna str)
  (let ([list-char (string->list str)])
    (list->string (map rna-map list-char))))