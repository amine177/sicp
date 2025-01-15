(define (make-monitored f)
  (define counter 0)
  (define (dispatch arg)
    (cond ((eq? 'how-many-calls? arg)
	   counter)
	  (else
	   (begin (set! counter (+ counter 1))
		  (f arg)))))
  dispatch)
    
(define s (make-monitored sqrt))
