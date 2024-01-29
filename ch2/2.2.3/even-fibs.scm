(define (fib n)
  (define (fib-iter fn-1 fn-2 i)
    (cond ((= i 0) fn-2)
	  (else (fib-iter (+ fn-1 fn-2) fn-1 (- i 1)))))
  (fib-iter 1 0 n))


(define (even-fibs n)
  (newline)
  (display n)
  (newline)

    (if (= n (- 1))
	'()
	(let ((fb (fib n)))
	  (if (even? fb)
	      (cons fb (even-fibs (- n 1)))
	      (even-fibs (- n 1))))))

(even-fibs 11)


(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (e-f n)
  (reduce
   cons
   '()
   (filter even? (map fib (enumerate-interval 0 n)))))
