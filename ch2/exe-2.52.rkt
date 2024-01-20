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
                            (make-vect 0 0.75))
                           (make-segment
                            (make-vect 0.40 0.93)
                            (make-vect 0.45 0.93))
                           (make-segment ; a
                            (make-vect 0.39 0.94)
                            (make-vect 0.40 0.93))))

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

(define wave
  (segments->painter wave-segments))

; b.
(define (diagonalize painter)
  (lambda (frame)
    (transform-painter
     painter
     (make-vect 0 0)
     (make-vect 0.75 0.25)
     (make-vect 0.25 0.75))))
     
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((upper (up-split painter (- n 1))))
        (below painter (beside upper upper)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((right-to (right-split painter (- n 1))))
        (beside painter right-to))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((upper (up-split painter (- n 1)))
            (right-to (right-split painter (- n 1))))
        (below
         (beside painter (diagonalize right-to))
         (beside
          upper
          (corner-split painter (- n 1)))))))

;c


(define (right-split-c painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split-c painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split-c painter n)
  (if (= n 0)
      painter
      (let ((upper (up-split-c painter (- n 1))))
        (below painter (beside upper upper)))))

(define (corner-split-c painter n)
  (if (= n 0)
      painter
      (let ((upper (up-split-c painter (- n 1)))
            (right-to (right-split-c painter (- n 1))))
        (below
         (beside painter (below right-to right-to))
         (beside (beside upper upper) (corner-split-c painter (- n 1)))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (below
     (beside (bl painter) (br painter))
     (beside (tl painter) (tr painter)))))

(define (triangle-of-three up left right)
  (lambda (painter)
    (below (up painter) (beside (left painter) (right painter)))))


(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  identity flip-horiz)))
    (combine4 (corner-split-c painter (- n 1)))))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.75 0.25)
                     (make-vect 0.25 0.75)))


(define (diagonal-split painter n)
  (if (= n 0)
      painter
        (let ((rest (diagonal-split painter (- n 1))))
          (below
           (squash-inwards painter) (beside rest rest)))))
  
(define (square-limit-tr painter n)
  (let ((combine3 (triangle-of-three
                   identity identity identity)))
        (combine3 (diagonal-split painter (- n 1)))))

