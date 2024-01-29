(define (enumerate-interval acc start end)
  (if (> start end)
      acc
      (enumerate-interval (append acc (list start)) (+ start 1) end)))


(define (flatmap fn sequence)
  (reduce append '() (map fn sequence)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position
		    new-row k rest-of-queens))
		 (enumerate-interval '() 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))


(define (adjoin-position row column other-positions)
;  (display "\nadjoin : ")
;  (display other-positions)
;  (display " and : ")
;  (display (list row column))
;  (newline)
;  (display " result : ")
;  (display (append other-positions (list row column)))
  (append (list (list row column)) other-positions))
  

(define empty-board '())

(define (safe? k positions)
;  (display "\n***\n")
;  (display positions)
;  (newline)
;  (display (car positions))
;  (display "\n***\n")
;  (newline)
  (cond ((null? positions) #t)
	((= (length positions) 1) #t)
	(else
	 (let ((tested-queen-row (car (car positions)))
	    (tested-queen-column (cadr (car positions)))
	    (other-queen-row (car (cadr positions)))
	    (other-queen-column (cadr (cadr positions))))
;	   (display "\n tested-queen-column: ")
;	   (display tested-queen-column)
;	   (display "\n tested-queen-row: ")
;	   (display tested-queen-row)
;	   (display "\n Other-queen-column: ")
;	   (display other-queen-column)
;  	   (display "\n Other-queen-row  ")
;	   (display other-queen-row)
;	   (newline)
	   (cond ((= other-queen-row tested-queen-row) #f)
		 ((= (abs (- tested-queen-column other-queen-column)) (abs (- tested-queen-row other-queen-row))) #f)
	      (else
	       (safe? k (append (list (car positions)) (cdr (cdr positions))))))))))
