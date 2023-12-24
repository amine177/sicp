(define (same-parity . args)
  (define (recurse rslt e . va)
    (let ((varargs (car va)))
      (cond ((null? varargs)
	   rslt)
	  (else
	   (if (=
		(bitwise-and e 1)
		(bitwise-and (car varargs) 1))
	       (recurse
		(append rslt (list (car varargs)))
		e
		(cdr varargs))
	       (recurse rslt e (cdr varargs)))))))
  (recurse (list (car args)) (car args) (cdr args)))
