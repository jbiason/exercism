#lang racket

(provide leap-year?)

(define (div-by-4 year)
  (= 0 (remainder year 4))
  )

(define (not-div-by-100 year)
  (not (= 0 (remainder year 100)))
  )

(define (div-by-400 year)
  (= 0 (remainder year 400))
  )

(define (leap-year? year)
  (and (div-by-4 year) 
       (or (div-by-400 year)
	    (not-div-by-100 year))
       )
  )
