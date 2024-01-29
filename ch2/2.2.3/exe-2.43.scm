; This runs slow because instead of mapping every set
; of combinations of (- k 1) queens on every row with every possible
; position of one queen  on all row.
; We are now for every possible position of one queen on all rows
; recalculating all the other combinations of (- k 1) queens on all rows
; (board-size * board-size) times
; T = O(n^3)
; T_exe_43 = O(n^(n+1))
(define (queen board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter (lambda (tested-positions-combination)
		  (safe? k tested-positions-combination))
		(flatmap (lambda (row)
			   (map (lambda (positions-combination)
				  (adjoin-position
				   row
				   k
				   positions-combination))
				(queen-cols (- k 1))))
			 (enumerate-interval '()
					     1
					     board-size)))))
  (queen-cols board-size))

