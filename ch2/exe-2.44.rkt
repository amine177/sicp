#lang sicp
(#%require sicp-pict)

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

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split wave 4))
(paint (right-split wave 5))
