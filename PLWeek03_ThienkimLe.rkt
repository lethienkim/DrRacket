#lang scheme

; Function to calculate the series
(define (customSeries n)
  (if (= n 0)
      0
      (+ (/ 1 (expt 4 n)) (customSeries (- n 1)))))

; Calculate the result for 20 terms
(define result (+ (expt 4 0) (customSeries 20)))

; Display the result
(displayln (exact->inexact result))
