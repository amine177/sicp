(define (same-parity . args)
  (define (iter rslt e . va)
    (let ((varargs (car va)))
      (cond ((null? varargs)
	   rslt)
	  (else
	   (if (=
		(bitwise-and e 1)
		(bitwise-and (car varargs) 1))
	       (iter
		(append rslt (list (car varargs)))
		e
		(cdr varargs))
	       (iter rslt e (cdr varargs)))))))
  (iter (list (car args)) (car args) (cdr args)))
