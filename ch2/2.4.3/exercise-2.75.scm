(define (make-from-mag-ang r phi)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
	  ((eq? op 'angle) phi)
	  ((eq? op 'real-part) (* (cos phi) r))
	  ((eq? op 'imag-part) (* (cos phi) r))
	  (else (error "unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define z (make-from-mag-ang 3 5))
(apply-generic 'real-part z)
(apply-generic 'imag-part z)
(apply-generic 'magnitude z)
(apply-generic 'angle z)
