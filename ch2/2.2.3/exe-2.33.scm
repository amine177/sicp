(define (map-reduce p sequence)
  (reduce (lambda (x y)
	    (cond ((null? y)
		   (cons y (p x)))
		  ((pair? y)
		   (cons y (p x)))
		  (else (cons (p y) (p x)))))
	  '()
	  sequence))

(mapp (lambda (x) (* x x)) (list 3 4 5))

(define (reverse-reduce seq)
  (reduce (lambda (x y)
	    (if (not (pair? y))
		(cons x (cons y '()))
		(cons x y))) '() seq))

(define (append-reduce seq1 seq2)
  (if (null? seq2)
      seq1
      (reduce cons '() (cons seq2 (reduce (lambda (x y)
					(if (not (pair? y))
					    (cons x (cons y '()))
					    (cons x y))) '() seq1)))))

(append-reduce (append-reduce (list 1 2) (list 3 5)) (list 6 7))

(define (length-reduce sequence)
  (reduce (lambda (x y)
		(+  1 (length-reduce (cdr sequence))))
	      0
	      sequence))
(length-reduce (list 1 2 3))


(define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
(define (map-accumulate p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(map-accumulate (lambda (x) (* x x)) (list 3 4 5))

(define (append-accumulate seq1 seq2)
  (accumulate cons seq2 seq1))

(append-accumulate (list 1 2) (list 3 4))

(define (length-accumulate sequence)
  (accumulate (lambda (x y) 
		(+ 1 y))
	      0
	      sequence))

(length-accumulate (list 1 2 3))
