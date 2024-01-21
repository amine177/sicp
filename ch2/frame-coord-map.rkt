#lang sicp
(#%require sicp-pict)
(#%require (for-syntax syntax/parse)
         racket/draw
         racket/snip

         racket/runtime-path)

(define-runtime-path fishin-file "fish.png")
(define fish         (bitmap->painter fishin-file))
(define wave
  (segments->painter (list (make-segment
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
                            (make-vect 0 0.75)))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))
