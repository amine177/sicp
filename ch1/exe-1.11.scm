(define (f n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(define (f-it n)
  (f-iter 0 1 2 n))

(define (f-iter a b c counter)
  (cond ((= counter 0) a)
	((= counter 1) b)
	((= counter 2) c)
	(else (f-iter b c (+ c (* 2 b) (* 3 a))  (- counter 1)))))

(display "recursive f:\n")
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)

(display "iterative f:\n")
(f-it 1)
(f-it 2)
(f-it 1)
(f-it 2)
(f-it 3)
(f-it 4)


(define (f_iter_internal  n a b c counter)
    (if (= counter (- n 2))
	a
	(if (< n 3)
	    n
	    (f_iter_internal
	       n
	       (+ a (* 2 b) (* 3 c))
	       a
	       b
	       (+ counter 1)))))
(define (f_iter x)
  (f_iter_internal x 2 1 0 0))

(f_iter 9)
