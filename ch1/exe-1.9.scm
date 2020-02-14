(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
 ; the process of evaluation
 ; (+ 4 5)
 ; (inc (+ 3 5)
 ; (inc (inc (+ 2 5)))
 ; (inc (inc (inc (+ 1 5))))
 ; (inc (inc (inc (inc (+ 0 5)))))
 ; (inc (inc (inc (inc 5))))
 ; (inc (inc (inc 6)))
 ; (inc (inc 7))
 ; (inc 8)
 ; 9
 
 
; this is a recursive process, because the evaluation
; takes the form of a chain of deferred calls to (inc )
; which needs the last call to pass its returned value to the previous call
; in order to complete the computation. 


(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
 ; the process of evaluation
 ; (+ 3 6)
 ; (+ 2 7)
 ; (+ 1 8)
 ; (+ 0 9)
 ; 9
; this is an iterative process because every new call to (+ ) doesn't need
; an ever growing chain calls in order to have a correct state
; state in this case is sufficiently described by the fixed amount of parameters
; passe to (+ ) on every call


; note that in both cases the procedure is recursive
; as it calls itself from within. Yet the nature of the process varies.
; Because a procedure is a syntactic model and process is an evolution model (execution)
