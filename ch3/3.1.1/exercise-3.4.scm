(define (make-account balance password)
  (define attempts 0)
  (define (call-cops)
    (error "Attempted more than 7 times invalid access, calling the cops!"))
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (cond ((>= balance 0)
	   (set! balance (+ balance amount))
	   balance)
	  (else "Negative deposit not allowed")))
  (define (dispatch pwd op)
    (cond ((eq? pwd password)
	   (begin (if (and (>= attempts 1) (<= attempts 6))
		      (set! attempts (- attempts 1))
		      (set! attempts 0)))
	    (cond ((eq? op 'withdraw) withdraw)
		  ((eq? op 'deposit) deposit)
		  (else (error "Unknown request: MAKE-ACCOUNT"
			       op))))
	  (else (begin
		  (set! attempts (+ attempts 1))
		  (cond ((>= attempts 7)
			 (set! attempts 7)
			 (call-cops))
			(else (lambda (holder) "Wrong authentication attempt!")))))))
  dispatch)


(define acc (make-account 100 'sicp))
((acc 'wrong-pass 'withdraw) 200)
((acc 'sicp 'withdraw) 200)
((acc 'sicp 'withdraw) 50)
