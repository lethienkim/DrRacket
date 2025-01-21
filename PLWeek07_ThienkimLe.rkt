#lang racket

;; Helper function to filter elements less than the pivot
(define (filter-less lst pivot)
  (cond
    [(empty? lst) '()]  ; Base case: empty list
    [(< (first lst) pivot) (cons (first lst) (filter-less (rest lst) pivot))]  ; Add to 'less' if less than pivot
    [else (filter-less (rest lst) pivot)]))  ; Skip if not less

;; Helper function to filter elements equal to the pivot
(define (filter-equal lst pivot)
  (cond
    [(empty? lst) '()]  ; Base case: empty list
    [(= (first lst) pivot) (cons (first lst) (filter-equal (rest lst) pivot))]  ; Add to 'equal' if equal to pivot
    [else (filter-equal (rest lst) pivot)]))  ; Skip if not equal

;; Helper function to filter elements greater than the pivot
(define (filter-greater lst pivot)
  (cond
    [(empty? lst) '()]  ; Base case: empty list
    [(> (first lst) pivot) (cons (first lst) (filter-greater (rest lst) pivot))]  ; Add to 'greater' if greater
    [else (filter-greater (rest lst) pivot)]))  ; Skip if not greater

;; Recursive function to sort a list without using prohibited functions
(define (mySort lst)
  (if (empty? lst)
      '()  ; Base case: an empty list is already sorted
      (let* ([pivot (first lst)]  ; Take the first element as the pivot
             ;; Partition the rest of the list into 'less', 'equal', and 'greater' lists based on the pivot
             [less (filter-less lst pivot)]
             [equal (filter-equal lst pivot)]
             [greater (filter-greater lst pivot)])
        ;; Recursively sort the 'less' and 'greater' lists, and append the results
        (append (mySort less) equal (mySort greater)))))

;; Test the function with the provided input
(displayln (mySort '(20 13 74 5 12 9 22 94 22 6 96 72 3 53 33 22 21 101 3 17 15 95 88)))

