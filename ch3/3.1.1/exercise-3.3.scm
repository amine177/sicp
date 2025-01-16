(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (cond ((>= balance 0)
	   (set !balance (+ balance amount))
	   balance)
	  (else "Negative deposit not allowed")))
  (define (dispatch pwd op)
    (cond ((eq? pwd password)
	    (cond ((eq? op 'withdraw) withdraw)
		  ((eq? op 'deposit) deposit)
		  (else (error "Unknown request: MAKE-ACCOUNT"
			       op))))
	  (else (error "Invalid password"))))
  dispatch)


(define acc (make-account 100 'sicp))
((acc 'wrong-pass 'withdraw) 200)
((acc 'sicp 'withdraw) 200)
((acc 'sicp 'withdraw) 50)
