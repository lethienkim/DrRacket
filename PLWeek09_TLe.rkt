#lang racket
(require racket/draw) ; Import graphics library

;; Step 1: Set up canvas dimensions and background
(define imageWidth 512)
(define imageHeight 288)
(define target (make-bitmap imageWidth imageHeight))
(define dc (new bitmap-dc% [bitmap target]))

;; Set the background color to purple
(send dc set-pen "green" 1 'solid) ; Border color
(send dc set-brush (make-color 100 30 130) 'solid) ; Background color
(send dc draw-rectangle 0 0 imageWidth imageHeight) ; Fill the background

;; Step 2: Define a polygon shape with more than three points
(define myPolygon (new dc-path%)) ; Create polygon
(send myPolygon move-to -10 -10) ; Set starting point
(send myPolygon line-to -5 10)
(send myPolygon line-to 5 10)
(send myPolygon line-to 10 -10)
(send myPolygon close)

;; HSV to RGB function to cycle through colors
(define (hsv->rgb hue s v)
  (define h (modulo hue 360)) ; Ensure hue is within the range 0-360 degrees
  (define c (* v s))
  (define h-prime (/ h 60.0))
  (define x (* c (- 1 (abs (- h-prime (floor h-prime) 1)))))
  (define m (- v c))
  (define-values (r g b)
    (cond
      [(< h-prime 1) (values c x 0)]
      [(< h-prime 2) (values x c 0)]
      [(< h-prime 3) (values 0 c x)]
      [(< h-prime 4) (values 0 x c)]
      [(< h-prime 5) (values x 0 c)]
      [(< h-prime 6) (values c 0 x)]
      [else (values 0 0 0)]))
  (make-color (exact-round (* 255 (+ r m)))
              (exact-round (* 255 (+ g m)))
              (exact-round (* 255 (+ b m)))))

;; Function to draw polygon with transformations
(define (draw-polygon-transformed dc polygon x y scale rotation color)
  (define temp-path (new dc-path%))
  (send temp-path append polygon)
  (send temp-path translate x y)
  (send temp-path scale scale scale)
  (send temp-path rotate rotation)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'solid)
  (send dc draw-path temp-path))

;; Step 3: Loop through rotations, translations, and color changes
(define (draw-rotating-polygons dc polygon num-steps)
  (for ([i (in-range num-steps)])
    (let* ([angle (* i 5)]                  ; Rotate by 5 degrees each step
           [x-shift (* i 3)]                ; Small incremental x-translation for spiral effect
           [y-shift (* i 2)]                ; Small incremental y-translation for spiral effect
           [hue (* i (/ 360.0 num-steps))]  ; Cycle hue within the color wheel
           [color (hsv->rgb hue 1.0 1.0)])  ; Convert HSV to RGB

      ;; Draw the rotated polygon with transformations and color
      (draw-polygon-transformed dc polygon
                                (+ 256 x-shift) (+ 144 y-shift) ; Position with incremental shift
                                1                                ; Scale factor (no additional scaling)
                                angle                            ; Rotation angle
                                color))))

;; Draw 90 polygons rotating through 360 degrees and cycling colors
(draw-rotating-polygons dc myPolygon 90)

;; Save the image
(send target save-file "polygon-demo.png" 'png)

;; Display the result in DrRacket if available
target
