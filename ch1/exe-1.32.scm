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

(define (accumulate-iter2 combiner term a next b accumulator)
  ; iter	
  (define (iter a b accumulator)
    (if (> a b)
	accumulator
    (iter (next a) b (combiner (term a) accumulator))))
  (iter a b accumulator))

(define (factorial n)
  (define (identity x)
    x)
  (define (next x)
    (+ x 1))
  ; *: combiner, identity: term, next: next
  (accumulate * 1 identity 1 next n))

(define (factorial-iter n)
  ; term
  (define (identity x)
    x)
  ; next
  (define (next x)
    (+ x 1))
  (accumulate-iter * identity 1 next n 1))

(define (factorial-iter2 n)
  ; term
  (define (identity x) 
    x)
  ; next
  (define (next x)
    (+ x 1))
  (accumulate-iter2 * identity 1 next n 1))

(factorial 5)
(factorial-iter 5)
(factorial-iter2 5)

(disassemble factorial)
