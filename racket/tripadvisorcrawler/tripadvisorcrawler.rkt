#lang racket
(require net/url
         json
         html-parsing
         webscraperhelper
         sxml)

;return the list of all related url for a city
;either near by or similar. Try paris or mumbai.
;if you want hotels from city you are looking for
; use first function to front element of list.
;top-url-list "mumbai"
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

;helper function for city-url-list
(define (create-url url n)
  (let*-values
      ([(p q) (split-at (string-split url "-") 2)]
       [(tlist) (append p (cons (string-append "oa"
                                               (number->string (* 30 (- n 1)))) empty) q)]
       [(ret) (string-join tlist "-")]) ret))


;you pass the url of city to fetch all the url link of each page
;related to city (number of pages on tripadvisor for the city)
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
           ([link (range (string->number (first page-num-list))
                         (+ 1 (string->number (last page-num-list))))]
            [ret (map (λ(x) (create-url url x)) link)])
         (cons url ret))])))


;write a function that fetch the name of hotel, review count, rank, and url
;function takes a url of tripadvisor page and return above mentioned data.
;This one is bit complecated because it executes java script so think for a
;method which executes java script racket.
(define (hotel-info url)
  (html->xexp
   (get-pure-port
    (string->url url))))


(hotel-info "https://www.tripadvisor.in/Hotels-g187147-oa1770-Paris_Ile_de_France-Hotels.html")