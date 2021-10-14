#lang racket

(provide square total)

(define (square val)
  (if (= val 1)
    1
    (* 2 (square (- val 1)))))

(define (total)
  18446744073709551615)
