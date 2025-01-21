#lang racket

;; Version 1: Using a named subprogram to determine the sign
(define (sign-fn n)
  (if (even? n) 1 -1))  ;; Returns 1 for even n and -1 for odd n

;; Recursive function to calculate the power series for 20 terms using the named subprogram for the sign
(define (power-series-named x)
  (define (helper k)
    (if (> k 20)   ;; Recursion stops after 20 terms
        0.0
        (+ (* (sign-fn k) (/ 1.0 (expt x k)))  ;; Alternating sign and division by x^k
           (helper (+ k 1)))))                 ;; Recursively calls itself with the next term
  (+ 1 (helper 0)))                            ;; Start the series with 1, then apply helper

;; Version 2: Using a lambda function directly to determine the sign
(define (power-series-lambda x)
  (define (helper k)
    (if (> k 20)   ;; Recursion stops after 20 terms
        0.0
        (+ (* ((lambda (k) (if (even? k) 1 -1)) k)  ;; Lambda determines the sign inline
              (/ 1.0 (expt x k)))                  ;; Alternating sign and division by x^k
           (helper (+ k 1)))))                     ;; Recursively calls itself with the next term
  (+ 1 (helper 0)))                                ;; Start the series with 1, then apply helper

(newline)

;; Testing power-series-named with input 3 and 4
(printf "Power series using named function with input 3: ~a\n" (power-series-named 3))
(printf "Power series using named function with input 4: ~a\n" (power-series-named 4))
(newline)

;; Testing power-series-lambda with input 3 and 4
(printf "Power series using lambda function with input 3: ~a\n" (power-series-lambda 3))
(printf "Power series using lambda function with input 4: ~a\n" (power-series-lambda 4))
