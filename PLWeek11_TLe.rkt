#lang racket 
(require racket/draw)
(require racket/random)

; Image dimensions
(define width 1920)
(define height 1080)

; Initialize bitmap and drawing context
(define my-bitmap (make-bitmap width height))
(define dc (new bitmap-dc% [bitmap my-bitmap]))

; Set background to white
(send dc set-brush "white" 'solid)
(send dc draw-rectangle 0 0 width height)

; Parameters
(define max-depth 18)                  ; Maximum recursion depth
(define branch-scale 0.67)             ; Scale factor for branch length
(define base-length (* height 0.12))  ; Trunk length
(define randomness 0.2)                ; Randomness factor for angles and lengths
(define angle-change (/ pi 6.5))       ; Angle between branches
(define min-branch-size 1.0e-12)         ; Minimum branch size to terminate
(define smallest-visible-size 1.0e-12) ; Visibility threshold
(define trunk-thickness 8)             ; Maximum trunk thickness

; Canvas center
(define center-x (/ width 2))
(define center-y (/ height 2))

; Statistics tracking
(define polygon-count 0)
(define largest-polygon 0)
(define smallest-polygon base-length)

; Update statistics
(define (update-statistics length)
  (when (>= length smallest-visible-size)
    (set! polygon-count (+ polygon-count 1)))
  (when (>= length 1)
    (set! largest-polygon (max largest-polygon length)))
  (set! smallest-polygon (min smallest-polygon length)))

; Helper functions
(define (angle-to-center x y)
  (atan (- center-y y) (- center-x x)))

; Mutable vector to store branches
(define branches (make-vector 0))

; Helper function to add a branch to the vector
(define (add-branch branch)
  (set! branches (vector-append branches (vector branch))))

; Seed the global random generator
(random-seed 123) ; Set seed for deterministic randomness

; Function to get a deterministic pseudo-random real number
(define (fixed-random)
  (/ (random 1000) 1000.0)) ; Produces a real number between 0 and 1

; Add branches starting from corners and edges pointing toward the canvas center
(add-branch (vector 10 10 base-length (angle-to-center 10 10) max-depth))                                      ; Top-left corner
(add-branch (vector 10 (- height 10) base-length (angle-to-center 10 (- height 10)) max-depth))               ; Bottom-left corner
(add-branch (vector (- width 10) (- height 10) base-length (angle-to-center (- width 10) (- height 10)) max-depth)) ; Bottom-right corner
(add-branch (vector (- width 10) (/ height 4) base-length (angle-to-center (- width 10) (/ height 4)) max-depth)) ; Above right edge
(add-branch (vector 10 (- (/ height 3) 10) base-length (angle-to-center 10 (- (/ height 3) 10)) max-depth))   ; Lower top-left corner
(add-branch (vector (- width 10) 10 base-length (angle-to-center (- width 10) 10) max-depth))                 ; Top-right corner

; Add branch starting from the center bottom of the canvas
;(add-branch (vector center-x (- height 10) base-length (- (/ pi 2)) max-depth)) ; Center-bottom branch



; Generate the fractal tree
(for ([depth (in-range max-depth)])
  (define new-branches (make-vector 0))
  (for ([branch (in-vector branches)])
    (let ([x (vector-ref branch 0)]
          [y (vector-ref branch 1)]
          [length (vector-ref branch 2)]
          [angle (vector-ref branch 3)]
          [depth (vector-ref branch 4)])
      (when (> depth 0)
        ; Update statistics
        (update-statistics length)

        ; Calculate endpoint of the branch
        (define end-x (+ x (* length (cos angle))))
        (define end-y (+ y (* length (sin angle))))

        ; Adjust pen width based on depth
        (define pen-width (inexact->exact 
                           (max 1 (round (* trunk-thickness (/ depth max-depth))))))

        ; Draw the branch
        (send dc set-pen "black" pen-width 'solid)
        (send dc draw-line x y end-x end-y)

        ; Calculate properties for new branches
        (define new-length (* length branch-scale (+ 1 (- (fixed-random) 0.5) randomness)))
        (when (> new-length min-branch-size)
          (when (< (fixed-random) 0.8) ; Reduced probability of branching
            (let ([left-angle (+ angle angle-change (* randomness (- (fixed-random) 0.5)))]
                  [right-angle (- angle angle-change (* randomness (- (fixed-random) 0.5)))])
              (set! new-branches (vector-append new-branches
                                                (vector (vector end-x end-y new-length left-angle (- depth 1))
                                                        (vector end-x end-y new-length right-angle (- depth 1)))))))))))
  (set! branches new-branches)) ; Update branches for the next iteration

; Save the image
(send my-bitmap save-file "fractal-branches.png" 'png)

; Print statistics
(printf "Polygon Count: ~a\n" polygon-count)
(printf "Largest Polygon : ~a\n" largest-polygon)
(printf "Smallest Polygon: ~a\n"
        (if (< smallest-polygon smallest-visible-size) "None" smallest-polygon))

; Return the bitmap to display in REPL
my-bitmap