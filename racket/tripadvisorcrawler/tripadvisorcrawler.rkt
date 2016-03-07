#lang racket
(require net/url
         json
         html-parsing
         webscraperhelper
         sxml)

;return the list of all related url for a city
;either near by or similar. Try paris or mumbai.
;if you want hotels from city you are looking for
; use first function to front element of list
(define (top-url-list city)
  (let* ([doc  (read-json
                (get-pure-port
                 (string->url
                  (string-append "https://www.tripadvisor.in/TypeAheadJson?query="
                                 city
                                 "&action=API&types=geo,theme_park&link_type=hotel&details=false"))))]
         [hot (hash-ref doc 'results)]
         [all-link (map (λ(x) (hash-ref x 'url)) hot)]
         [ret (map (λ(x) (string-append "https://www.tripadvisor.in" x)) all-link)])
    ret))


;you pass the url of city to fetch all the url links
;related to city (number of pages on tripadvisor)
(define (city-url-list url)
  (let* ([page (html->xexp
                (get-pure-port
                 (string->url url)))]
         [doc ((sxpath "(//div[@class=\"pageNumbers\"]/a[@href])") page)]
         [page-num (map (λ(x) (sxml:attr x 'data-page-number)) doc)])
         (match page-num
           ['() (cons url empty)]
           [page-num-list
            (let*
                ([link (range (string->number (first page-num-list)) (+ 1 (string->number (last page-num-list))))]
                 [ret (map (λ(x) (string-append "https://www.tripadvisor.in"
                                                   "/Hotels-g304554-oa"
                                                   (number->string (* 30 (- x 1)))
                                                   "-Mumbai_Bombay_Maharashtra-Hotels.html")) link)])
              (cons url ret))])))
         
(define doc
  (city-url-list "https://www.tripadvisor.in/Hotels-g3458458-Portland_Parish_Jamaica-Hotels.html"))

;write a function that fetch the name of hotel and its review url
;(struct 
