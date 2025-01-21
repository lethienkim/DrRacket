#lang racket

;; Tail-recursive function to count the number of items in a list without using a helper
(define (listLength lst)
  (cond
    [(empty? lst) 0]                      ; Base case: if the list is empty, return 0
    [else (+ 1 (listLength (cdr lst)))])) ; Recursive case: count the current element and recurse on the rest of the list

;; Recursive function to count the number of atoms (non-list elements) in a list without using a helper
(define (deepListLength lst)
  (cond
    [(empty? lst) 0]                      ; Base case: if the list is empty, return 0
    [(list? (car lst))                    ; If the first element is a list, recursively count its elements
     (+ (deepListLength (car lst)) (deepListLength (cdr lst)))]
    [else (+ 1 (deepListLength (cdr lst)))])) ; If the first element is not a list, count it and recurse

;; Examples to test listLength   
(listLength '(1 2 3 4))               ; Output: 4
(listLength '(1 (2 3) (4 (5 6))))     ; Output: 3 (only counts top-level elements)

;; Examples to test deepListLength
(deepListLength '(1 2 3 4))               ; Output: 4 (counts all atoms)
(deepListLength '(1 (2 3) (4 (5 6))))     ; Output: 6 (counts all atoms, even inside nested lists)