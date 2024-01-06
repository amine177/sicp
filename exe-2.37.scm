(define (accumulate op initial sequence)
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial sequences)
  (if (null? (car sequences))
      '()
      (cons (accumulate op initial (map (lambda (x)
					(car x))
				      sequences))
	  (accumulate-n op initial (map (lambda (x)
					  (cdr x))
					sequences)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m n)
  (map (lambda (x)
	 (dot-product x n)) m))

(define m (list (list 1 2 3 4)
		(list 4 5 6 6)
		(list 6 7 8 9)))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((n-transpose (transpose n)))
    (map (lambda (x)
	   (matrix-*-vector n-transpose x)) m)))

(matrix-*-vector m (list 1 1 1 1))
(define 1-by-1 (list (list 1 1 1) (list 1 1 1) (list 2 2 2 )))
(matrix-*-matrix 1-by-1 1-by-1)

