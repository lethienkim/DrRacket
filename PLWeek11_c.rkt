#lang racket
(require racket/draw)

; Image dimensions
(define width 1920)
(define height 1080)

; Initialize bitmap and drawing context
(define my-bitmap (make-bitmap width height))
(define dc (new bitmap-dc% [bitmap my-bitmap]))

; Set background to white
(send dc set-brush "white" 'solid)
(send dc draw-rectangle 0 0 width height)

(define max-depth 17)          ; Maximum depth of recursion
(define branch-scale 0.63)        ; Scale factor for branch length
(define base-length 200)         ; Initial trunk length
(define randomness 0.2)          ; Randomness in angles and lengths
(define angle-change (/ pi 7)) ; Angle to turn 

(define polygon-count 0)         ; Counter for polygons


; Mutable vector to store branches
(define branches (make-vector 0))



; Helper function to add a branch to the vector
(define (add-branch branch)
  (set! branches (vector-append branches (vector branch))))

; Initialize the first branch
(add-branch (vector (/ width 8) height base-length (- (/ pi 5)) max-depth))

; Generate the fractal tree using a loop
(for ([depth (in-range max-depth)])
  (define new-branches (make-vector 0))
  (for ([branch (in-vector branches)])
    (let ([x (vector-ref branch 0)]
          [y (vector-ref branch 1)]
          [length (vector-ref branch 2)]
          [angle (vector-ref branch 3)]
          [depth (vector-ref branch 4)])
      (when (> depth 0)
        ; Calculate endpoint of the branch
        (define end-x (+ x (* length (cos angle))))
        (define end-y (+ y (* length (sin angle))))
        
        ; Draw the branch
        (send dc set-pen "black" (max 1 (round (* 2 (/ depth max-depth)))) 'solid)
        (send dc draw-line x y end-x end-y)

        ; Increment polygon count
        (set! polygon-count (+ polygon-count 1))

        ; Calculate properties for new branches
        (define new-length (* length branch-scale (+ 1 (- (random) 0.5) randomness)))
        (define left-angle (+ angle angle-change (* randomness (- (random) 0.5))))
        (define right-angle (- angle angle-change (* randomness (- (random) 0.5))))

        ; Add left and right branches to new vector
        (set! new-branches
              (vector-append new-branches
                             (vector (vector end-x end-y new-length left-angle (- depth 1))
                                     (vector end-x end-y new-length right-angle (- depth 1))))))))
  (set! branches new-branches)) ; Update branches for next iteration

; Save the image
(define file-path "unique_fractal.png")
(send my-bitmap save-file file-path 'png)

; Print statistics
(displayln (string-append "Total polygons: " (number->string polygon-count)))


; Display the image in REPL
my-bitmap