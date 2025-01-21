#lang racket

;; The partition function splits the input list into three parts: 
;; elements less than the pivot, elements equal to the pivot, and elements greater than the pivot.
(define (partition lst pivot)
  
  ;; Helper function to accumulate elements into 'less', 'equal', and 'greater' lists.
  ;; It processes each element of the list and puts it in the appropriate category.
  (define (helper lst less equal greater)
    (cond
      ;; If the list is empty, return the three accumulated lists (less, equal, greater).
      [(empty? lst) (list less equal greater)]
      
      ;; If the first element is less than the pivot, add it to the 'less' list.
      [(< (first lst) pivot)
       (helper (rest lst) (cons (first lst) less) equal greater)]
      
      ;; If the first element equals the pivot, add it to the 'equal' list.
      [(= (first lst) pivot)
       (helper (rest lst) less (cons (first lst) equal) greater)]
      
      ;; If the first element is greater than the pivot, add it to the 'greater' list.
      [else
       (helper (rest lst) less equal (cons (first lst) greater))]))
  
  ;; Initialize the partitioning process by starting with empty 'less', 'equal', and 'greater' lists.
  (helper lst '() '() '()))

;; The mySort function implements a recursive sorting algorithm similar to QuickSort.
(define (mySort lst)
  ;; Base case: if the list is empty, it is already sorted, so return the empty list.
  (if (empty? lst)
      '()
      
      ;; Recursive case: partition the list around the pivot and sort each part.
      (let* ([pivot (first lst)]                 ;; Select the first element as the pivot.
             [partitioned (partition lst pivot)] ;; Partition the list into less, equal, and greater elements.
             [less (first partitioned)]          ;; The list of elements less than the pivot.
             [equal (second partitioned)]        ;; The list of elements equal to the pivot.
             [greater (third partitioned)])      ;; The list of elements greater than the pivot.
        
        ;; Recursively sort the 'less' and 'greater' lists, and then combine them with 'equal'.
        (append (mySort less) equal (mySort greater)))))

;; Test case to sort a list of numbers. This will display the sorted list.
(displayln (mySort '(20 13 74 5 12 9 22 94 22 6 96 72 3 53 33 22 21 101 3 17 15 95 88)))
