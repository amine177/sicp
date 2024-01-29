(define (for-each fn args)
  (cond ((null? args) #t)
	(else
	 (fn (car args))
	 (for-each fn (cdr args)))))

(for-each (lambda (x)  (newline) (display x)) (list 5 23 8))
