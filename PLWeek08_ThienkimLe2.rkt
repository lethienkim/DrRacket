#lang racket
;; Modified tree-insert to handle cases where the tree is incomplete
(define (tree-insert n tree)
  (cond
    ;; Base case: If the tree is empty, return a tree with n as the root, and two empty subtrees.
    [(null? tree) (list n '() '())]
    
    ;; If n is less than or equal to the root, insert into the left subtree.
    [(<= n (car tree))
     (list (car tree)
           (tree-insert n (cadr tree))  ; Insert into left subtree
           (caddr tree))]  ; Keep right subtree intact
    
    ;; Otherwise, insert into the right subtree.
    [else
     (list (car tree)
           (cadr tree)  ; Keep left subtree intact
           (tree-insert n (caddr tree)))]))  ; Insert into right subtree

;; Function to insert all numbers from the list into the tree
(define (list-to-tree lst tree)
  (if (null? lst)
      tree
      (list-to-tree (cdr lst) (tree-insert (car lst) tree))))

;; Test case
(define test-list '(22 25 7 16 8 34 67 7 32 17 8 4 5 3 19))
(define test-tree '())

(displayln (list-to-tree test-list test-tree))
