(define (accumulate combiner null-value term a next b)
  (if (> a b)
	 null-value
	 (combiner
	  (term a)
	  (accumulate combiner null-value term (next a) next b))))


(define (accumulate-iter combiner term a next b accumulator)
  (if (> a b)
      accumulator
      (accumulate-iter
       combiner
       term
       (next a)
       next
       b
       (combiner (term a) accumulator))))
