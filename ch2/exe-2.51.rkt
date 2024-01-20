#lang sicp
(#%require sicp-pict)

(#%require (for-syntax syntax/parse)
         racket/draw
         racket/snip
         racket/runtime-path)

(define-runtime-path fishin-file "fish.png")
(define fish         (bitmap->painter fishin-file))
(define wave-segments (list (make-segment
                            (make-vect 0.40 1.00)
                            (make-vect 0.35 0.95)) ; head left top line
                           (make-segment
                            (make-vect 0.45 1.00)
                            (make-vect 0.50 0.95)) ; head left bottom line
                           (make-segment
                            (make-vect 0.35 0.95)
                            (make-vect 0.4 0.90)) ; head right top line
                           (make-segment
                            (make-vect 0.50 0.95)
                            (make-vect 0.45 0.90)) ; head right bottom line
                           (make-segment
                            (make-vect 0.40 0.90)
                            (make-vect 0.30 0.905)) ; arm shoulder left
                           (make-segment
                            (make-vect 0.30 0.905)
                            (make-vect 0.1 0.85)) ; arm arm left
                           (make-segment
                            (make-vect 0.1 0.85)
                            (make-vect 0.00 0.80)) ; arm left hand
                           (make-segment
                            (make-vect 0.45 0.90)
                            (make-vect 0.55 0.90))
                           (make-segment
                            (make-vect 0.55 0.90)
                            (make-vect 0.75 0.845))
                           (make-segment
                            (make-vect 0.75 0.845)
                            (make-vect 1 0.90))
                           (make-segment
                            (make-vect 0.75 0.800)
                            (make-vect 1 0.865))
                           (make-segment
                            (make-vect 0.5  0.845)
                            (make-vect 0.75 0.800))
                           (make-segment
                            (make-vect 0.5  0.845)
                            (make-vect 0.5 0.7))
                           (make-segment
                            (make-vect 0.5 0.7)
                            (make-vect 0.6 0.5))
                           (make-segment
                            (make-vect 0.6 0.5)
                            (make-vect 0.5 0))
                           (make-segment
                            (make-vect 0.4 0)
                            (make-vect 0.5 0.5))
                           (make-segment
                            (make-vect 0.5 0.5)
                            (make-vect 0.4 0.7))
                           (make-segment
                            (make-vect 0.4 0.7)
                            (make-vect 0.3 0.5))
                           (make-segment
                            (make-vect 0.3 0.7)
                            (make-vect 0.2 0.5))
                           (make-segment
                            (make-vect 0.3 0.5)
                            (make-vect 0.4 0))
                           (make-segment
                            (make-vect 0.2 0.5)
                            (make-vect 0.3 0))
                           (make-segment
                            (make-vect 0.3 0.7)
                            (make-vect 0.3 0.845))
                           (make-segment
                            (make-vect 0.3 0.845)
                            (make-vect 0.1 0.80))
                           (make-segment
                            (make-vect 0.1 0.8)
                            (make-vect 0 0.75))))
(define wave
  (segments->painter wave-segments))
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((frame-map (frame-coord-map frame)))
      (let ((transformed-origin (frame-map origin)))
        (painter (make-frame
                  transformed-origin
                  (vector-sub
                   (frame-map corner1)
                   transformed-origin)
                  (vector-sub
                   (frame-map corner2)
                   transformed-origin)))))))



(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (rotate90-clockwise painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (rotate90-counter-clockwise painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.75 0.25)
                     (make-vect 0.25 0.75)))



(define (beside painter1 painter2)
  (let ((split-pt (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-pt
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-pt
                              (make-vect 1.0 0.0)
                              (make-vect (vector-xcor split-pt) 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
          
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
(define (below painter1 painter2)
  (let ((split-pt (make-vect 0.0 0.5)))
        (let ((paint-below
               (transform-painter painter1
                                  (make-vect 0.0 0.0)
                                  (make-vect 1.0 0.0)
                                  split-pt))
              (paint-top
               (transform-painter painter2
                                  split-pt
                                  (make-vect
                                   1.0
                                   (vector-ycor split-pt))
                                  (make-vect 0.0 1.0))))
          (lambda (frame)
            (paint-below frame)
            (paint-top frame)))))

(define (below-rot painter1 painter2)
  (lambda (frame)
    ((rotate90-clockwise (beside (rotate90-counter-clockwise painter2)
                 (rotate90-counter-clockwise painter1))) frame)))
