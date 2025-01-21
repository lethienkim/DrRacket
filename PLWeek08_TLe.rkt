#lang racket

;; Part 1: tree-insert handling four cases explicitly
(define (tree-insert n tree)
  (cond
    ;; Case 1: Empty tree
    [(null? tree)
     (list n)]
    
    ;; Case 2: Single node with no branches
    [(= 1 (length tree))
     (if (and (number? (car tree)) (<= n (car tree)))
         (list (car tree) (list (list n)) '())    ; Add left branch, right is empty
         (list (car tree) (list (list n))))]  ; Add right branch
    
    ;; Case 3: Node with one branch (left branch)
    [(= 2 (length tree))
     (if (and (number? (car tree)) (<= n (car tree)))
         (list (car tree) (tree-insert n (cadr tree)) '())  ; Insert or recurse on left
         (list (car tree) (cadr tree) (list (list n))))]    ; Add right branch
    
    ;; Case 4: Node with two branches (left and right)
    [(= 3 (length tree))
     (if (and (number? (car tree)) (<= n (car tree)))
         (list (car tree) (tree-insert n (cadr tree)) (caddr tree))  ; Recurse on left
         (list (car tree) (cadr tree) (tree-insert n (caddr tree))))] ; Recurse on right

    ;; Default case to handle unexpected structures safely
    [else tree]))



;; Part 2: list-to-tree
(define (list-to-tree lst tree)
  (foldl tree-insert tree lst))  ;; Accumulate tree by inserting each element from lst

;; Part 3: tree-to-list
(define (tree-to-list tree)
  (if (null? tree)
      '()
      (append (if (and (not (null? (cdr tree))) (>= (length tree) 2)) ;; Traverse left subtree only if it exists
                  (tree-to-list (cadr tree))
                  '())
              (list (car tree))                                      ;; Visit the root node
              (if (and (not (null? (cdr tree))) (>= (length tree) 3)) ;; Traverse right subtree only if it exists
                  (tree-to-list (caddr tree))
                  '()))))

;; Test cases
(tree-insert 8 '())             ;‘(8)	
(tree-insert 12 '(8))           ;'(8 ((12)))
(tree-insert 3 '(8))            ;'(8 ((3) ()))
(tree-insert 12 '(8 ((3) ())))  ;'(8 ((3) (12)))
(tree-insert 4 '(8 ((3) (12)))) ;'(8 ((3 ((4))) (12)))


(newline)
(list-to-tree '(22 25 7 16 8 34 67 7 32 17 8 4 5 3 19) '())
; Expected output: '(22 ((7 ((7 ((4 ((3) (5))) ())) (16 ((8 ((8) ())) (17 ((19))))))) (25 ((34 ((32) (67)))))))

(newline)
(tree-to-list (list-to-tree '(22 25 7 16 8 34 67 7 32 17 8 4 5 3 19) '()))
; Expected output: '(3 4 5 7 7 8 8 16 17 19 22 25 32 34 67)

