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

; Parameters for the L-shaped fractal tree
(define max-depth 18)                  ; Maximum recursion depth
(define branch-scale 0.78)              ; Scale factor for branch length
(define base-length (* height 0.2))    ; Initial trunk length
(define angle-change (/ pi 6.5))         ; Angle between branches
(define min-branch-size 1e-12)         ; Minimum branch size
(define scale-factor (min (/ width 4000) (/ height 3000)))
(define polygon-count 0)               ; Counter for polygons
(define smallest-polygon base-length)  ; Track smallest polygon size
(define largest-polygon 0)             ; Track largest polygon size

; Mutable vector to store branches
(define branches (make-vector 0))

; Helper function to add a branch to the vector
(define (add-branch branch)
  (set! branches (vector-append branches (vector branch))))

; Initialize the first branch (the trunk)
(add-branch (vector (/ width 2) (* height 0.95) base-length (- (/ pi 2)) max-depth))

; Generate the fractal tree using the L-shape logic
(define (generate-fractal)
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

          ; Adjust pen width based on depth
          (define pen-width (inexact->exact (round (max 1 (/ length 7)))))

          ; Draw the branch (L-shape)
          (send dc set-pen "black" pen-width 'solid)
          (send dc draw-line x y end-x end-y)

          ; Update polygon count
          (set! polygon-count (+ polygon-count 1))

          ; Update smallest and largest polygon sizes
          (set! smallest-polygon (min smallest-polygon length))
          (set! largest-polygon (max largest-polygon length))

          ; Calculate properties for new branches with slight variations
          (define new-length (* length branch-scale (+ 0.9 (/ (random 20) 100.0))))
          (define left-angle (+ angle angle-change (+ -0.05 (/ (random 10) 100.0))))
          (define right-angle (- angle angle-change (+ -0.05 (/ (random 10) 100.0))))

          ; Ensure new length is above minimum branch size
          (when (> new-length min-branch-size)
            ; Add new branches to the vector
            (set! new-branches (vector-append new-branches
                                              (vector (vector end-x end-y new-length left-angle (- depth 1))
                                                      (vector end-x end-y new-length right-angle (- depth 1)))))))))
    (set! branches new-branches))) ; Update branches for the next iteration

; Generate the fractal tree
(generate-fractal)

; Ensure the minimum polygon count
(when (< polygon-count 50000)
  (begin
    (displayln "Warning: Minimum polygon count not reached. Increasing depth.")
    (set! max-depth (+ max-depth 2))
    (generate-fractal)))

; Save the image
(define file-path "natural_l_shape_fractal_tree.png")
(send my-bitmap save-file file-path 'png)

; Print statistics
(displayln (format "Total polygons: ~a" polygon-count))
(displayln (format "Largest polygon size: ~a" largest-polygon))
(displayln (format "Smallest polygon size: ~a" smallest-polygon))

; Return the bitmap for viewing
my-bitmap
