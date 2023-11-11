(define (inc x)
  (+ x 1))

(define (double proc)
  (lambda (x) (proc (proc x))))

((double inc) 1) ; (inc ... inc : 2^1


(((double (double double)) inc) 0) ; (inc .... inc : 2^3
					; (((double double) ((double double) inc)) 5)
					; (((double double) (double (double inc))) 5)
					; ((double (double (double (double inc)))) 5)
					; ((double (double (double (inc (inc ))))) 5)
					; ((double (double (inc (inc (inc (inc)))))) 5)
					; ((double (inc (inc (inc (inc (inc (inc (inc (inc))))))))) 5)
					; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))



