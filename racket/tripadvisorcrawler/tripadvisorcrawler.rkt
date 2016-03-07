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
;method which executes java script in  racket. For the momenet, save the page in
;browser and do the parsing. When I have solution then replace it with url.
(define (hotel-info url)
  (html->xexp
   (get-pure-port
    (string->url url))))

;helper function to extract name, review url, number of reviews, and
; rating. Add data algebraic data structure

(struct hotel (name url num-reviews rating))

(define (page-struct-info div-node)
  (let* ([page-title ((sxpath "(//div[@class=\"listing_title\"]/a)/text()") div-node)]
         [page-url (sxml:attr (first ((sxpath "(//div[@class=\"listing_title\"]/a)") div-node)) 'href)]
         [num-review ((sxpath "(//span[@class=\"more\"]/a)/text()") div-node)]
         [tripadvisor-rank ((sxpath "(//div[@class=\"slim_ranking\"])/text()") div-node)])
    (list page-title page-url num-review tripadvisor-rank)))
         
;(hotel-info "https://www.tripadvisor.in/Hotels-g60713-oa30-San_Francisco_California-Hotels.html")
;go with html parsing and plug it later the url
(define (hotel-info-page page)
  (let* ([xexp-info-page
          (html->xexp (open-input-file page))]
         [doc ((sxpath "(//div[@class=\"listing_info popIndexValidation\"])") xexp-info-page)]
         [info-list (map page-struct-info doc)])
    info-list))
(hotel-info-page "SanFranciscoTripAdvisor.html")