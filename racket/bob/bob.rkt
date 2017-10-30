#lang racket

(provide response-for)

(define (response-for s)
  (cond
    [(string=? (string-trim s) "") "Fine. Be that way!"]
    [(and
      (string=? s (string-upcase s))
      (not (string=? s (string-downcase s)))) "Whoa, chill out!"]
    [(string-suffix? s "?") "Sure."]
    [else "Whatever."]))

