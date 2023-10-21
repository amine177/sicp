
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))
(define (sum a b)			; redefining + later will
					;result in an infinte call
  (if (= a 0) b				; chain so instead of
      (inc (sum (dec a)  b))))		; redefining + like the book
					;suggests we call it sum


(sum 4 5)				; (sum 4 5)
					; (inc (sum (dec 4) 5))
					; (inc (sum 3 5))
					; (inc (inc (sum (dec 3) 5))
					; (inc (inc (sum 2 5)))
					; (inc (inc (inc (sum (dec 2)
					; 5))))
					; (inc (inc (inc (sum 1 5))))
					; (inc (inc (inc (inc (sum
					; (dec 1 ) 5)))))
					; (inc (inc (inc (inc
					; (sum 0 5)))))
					; (inc (inc (inc (inc 5))))
					; (inc (inc (inc 6)))
					; (inc (inc 7))
					; (inc 8)
					; Value: 9

					; this evaluation creates
					; a recursive process as
					; we need to maintain how many
					; call to inc has been done


(define (sum_i a b)
  (if (= a 0) b
      (sum_i (dec a) (inc b))))

(sum_i 4 5)				; (sum 4 5)
					; (sum_i (dec 4) (inc 5))
					; (sum_i 3 6)
					; (sum_i (dec 3) (inc 6))
					; (sum_i 2 7)
					; (sum_i (dec 2) (inc 7))
					; (sum_i 1 8)
					; (sum_i (dec 1) (inc 8))
					; (sum_i 0 9)
					; Value: 9
					; this evluation creates
					; an iterative process as we
					; only keep track of
					; the parameters to sum_i


